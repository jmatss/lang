// TODO: Add array indexing here.
// TODO: Is it possible to convert the indexing with a ident to a number?
//       Ex. if "x" is the second member of a struct, it would be converted
//       to "1", or at least get it as a attribute. This is currently done in
//       in "TypeAnalyzer" but should probably be done here if possible.

use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        block::BlockHeader,
        expr::{AccessInstruction, Expression, FunctionCall, RootVariable, StructInit},
        op::{BinaryOperation, BinaryOperator, Operation, UnaryOperation, UnaryOperator},
        stmt::Statement,
    },
};
use parse::token::{ParseToken, ParseTokenKind};

pub struct IndexingAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> IndexingAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find information
    /// related to indexing of variables. This includes array indexing and also
    /// struct indexing.
    pub fn analyze(
        context: &'a mut AnalyzeContext,
        ast_root: &mut ParseToken,
    ) -> Result<(), Vec<LangError>> {
        let mut block_analyzer = IndexingAnalyzer::new(context);
        block_analyzer.analyze_indexing(ast_root);
        if block_analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut block_analyzer.errors))
        }
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        // Reset the `cur_block_id` to the default block (== 0).
        context.cur_block_id = 0;
        Self {
            context,
            errors: Vec::default(),
        }
    }

    fn analyze_indexing(&mut self, token: &mut ParseToken) {
        self.context.cur_line_nr = token.line_nr;
        self.context.cur_column_nr = token.column_nr;

        match token.kind {
            ParseTokenKind::Block(ref mut block_header, id, ref mut body) => {
                self.context.cur_block_id = id;

                match block_header {
                    BlockHeader::IfCase(expr_opt) => {
                        if let Some(expr) = expr_opt {
                            self.analyze_expr(expr);
                        }
                    }
                    BlockHeader::Match(expr) => {
                        self.analyze_expr(expr);
                    }
                    BlockHeader::MatchCase(expr) => {
                        self.analyze_expr(expr);
                    }
                    BlockHeader::For(_, expr) => {
                        self.analyze_expr(expr);
                    }
                    BlockHeader::While(expr_opt) => {
                        if let Some(expr) = expr_opt {
                            self.analyze_expr(expr);
                        }
                    }
                    _ => (),
                }

                for token in body {
                    self.analyze_indexing(token);
                }
            }
            ParseTokenKind::Statement(ref mut stmt) => self.analyze_stmt(stmt),
            ParseTokenKind::Expression(ref mut expr) => self.analyze_expr(expr),
            ParseTokenKind::EndOfFile => (),
        }
    }

    fn analyze_stmt(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Yield(expr) => self.analyze_expr(expr),
            Statement::Defer(expr) => self.analyze_expr(expr),
            Statement::DeferExecution(expr) => self.analyze_expr(expr),
            Statement::Assignment(_, lhs, rhs) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr);
                }
            }
            Statement::VariableDecl(_, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr);
                }
            }

            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::ExternalDecl(_)
            | Statement::Modifier(_) => (),
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expression) {
        match expr {
            Expression::FunctionCall(func_call) => self.analyze_func_call(func_call),
            Expression::StructInit(struct_init) => self.analyze_struct_init(struct_init),
            Expression::ArrayInit(args) => {
                for arg in args {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::Operation(op) => self.analyze_op(op),

            Expression::Literal(..) | Expression::Type(_) | Expression::Variable(_) => (),
        }
    }

    fn analyze_func_call(&mut self, func_call: &mut FunctionCall) {
        for arg in func_call.arguments.iter_mut() {
            self.analyze_expr(&mut arg.value);
        }
    }

    fn analyze_struct_init(&mut self, struct_init: &mut StructInit) {
        for arg in struct_init.arguments.iter_mut() {
            self.analyze_expr(&mut arg.value);
        }
    }

    fn analyze_op(&mut self, op: &mut Operation) {
        match op {
            Operation::BinaryOperation(bin_op) => self.analyze_bin_op(bin_op),
            Operation::UnaryOperation(un_op) => self.analyze_un_op(un_op),
        }
    }

    /// Set any variables that are used as lhs in a "Dot" bin op as a struct
    /// and all variables used in rhs of a "Dot" as a struct member.
    /// The vars will be found recursively.
    fn analyze_bin_op(&mut self, bin_op: &mut BinaryOperation) {
        let block_id = self.context.cur_block_id;

        // Analyze the left hand side before the Dot logic and then analyze
        // the right hand side after the Dot logic. This will ensure that
        // it is evaluated left-to-right which will allow one to access find
        // the "root variable" from the left before evaluating the right.
        self.analyze_expr(&mut bin_op.left);

        if let BinaryOperator::Dot = bin_op.operator {
            if let Some(lhs) = bin_op.left.eval_to_var() {
                // If the right is a variable, this is a struct member access.
                // Else if the right is a function call, this is a use of a method
                // belonging to the struct variable at the left hand side.
                if let Some(rhs) = bin_op.right.eval_to_var() {
                    let struct_var_name = lhs.name.clone();
                    let member_name = rhs.name.clone();

                    let access_instr =
                        AccessInstruction::StructMember(struct_var_name.clone(), member_name, None);

                    // If true: Nested struct, else: just a basic struct member.
                    // The index isn't set here, it will be set during TypeAnalyzing
                    // when the struct declarations are analyzed.
                    if let Some(ref lhs_access_instrs) = lhs.access_instrs {
                        let mut member_access_instrs = lhs_access_instrs.clone();
                        member_access_instrs.1.push(access_instr);
                        member_access_instrs.0.is_struct = true;

                        rhs.access_instrs = Some(member_access_instrs);
                    } else {
                        let var_decl_id =
                            match self.context.get_var_decl_scope(&struct_var_name, block_id) {
                                Ok(var_decl_id) => var_decl_id,
                                Err(e) => {
                                    self.errors.push(e);
                                    return;
                                }
                            };

                        let root_var = RootVariable::new(struct_var_name, var_decl_id, true);
                        rhs.access_instrs = Some((root_var, vec![access_instr]));
                    }
                } else if let Some(rhs_method_call) = bin_op.right.eval_to_func_call() {
                    let struct_var_name = lhs.name.clone();
                    let method_name = rhs_method_call.name.clone();

                    // The first argument is the struct name and it will be set
                    // during "method analyzing" since the type/struct are at
                    // this stage not known, it will be known after "type analyzing"
                    // has been ran.
                    let access_instr = AccessInstruction::StructMethod(None, method_name);

                    // If true: Nested struct, else: just a basic struct member.
                    if let Some(ref lhs_access_instrs) = lhs.access_instrs {
                        let mut method_access_instrs = lhs_access_instrs.clone();
                        method_access_instrs.1.push(access_instr);
                        method_access_instrs.0.is_struct = true;

                        rhs_method_call.access_instrs = Some(method_access_instrs);
                    } else {
                        let var_decl_id =
                            match self.context.get_var_decl_scope(&struct_var_name, block_id) {
                                Ok(var_decl_id) => var_decl_id,
                                Err(e) => {
                                    self.errors.push(e);
                                    return;
                                }
                            };

                        let root_var = RootVariable::new(struct_var_name, var_decl_id, true);
                        rhs_method_call.access_instrs = Some((root_var, vec![access_instr]));
                    }
                }
            } else {
                let err_msg = format!(
                    "Left hand side of Dot symbol didn't eval to variable. \
                    Left: {:?}, right: {:?}",
                    bin_op.left, bin_op.right
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return;
            }
        }

        self.analyze_expr(&mut bin_op.right);
    }

    /// Analyzes a unary operation. If it is a deref/address/array access it also
    /// adds a "AccessInstruction" to the Variable contain in the `un_op.value`.
    fn analyze_un_op(&mut self, un_op: &mut UnaryOperation) {
        let block_id = self.context.cur_block_id;

        self.analyze_expr(&mut un_op.value);

        if let Some(var) = un_op.value.eval_to_var() {
            let access_instr_opt = match &mut un_op.operator {
                UnaryOperator::Deref => Some(AccessInstruction::Deref),
                UnaryOperator::Address => Some(AccessInstruction::Address),
                UnaryOperator::ArrayAccess(ref mut dim) => {
                    self.analyze_expr(dim);
                    Some(AccessInstruction::ArrayAccess(*dim.clone()))
                }
                _ => None,
            };

            if let Some(access_instr) = access_instr_opt {
                if let Some(ref old_access_instrs) = var.access_instrs {
                    let mut new_access_instrs = old_access_instrs.clone();
                    new_access_instrs.1.push(access_instr);

                    var.access_instrs = Some(new_access_instrs);
                } else {
                    let var_decl_id = match self.context.get_var_decl_scope(&var.name, block_id) {
                        Ok(var_decl_id) => var_decl_id,
                        Err(e) => {
                            self.errors.push(e);
                            return;
                        }
                    };

                    // TODO: Does the `root_var` need to be set in the if-case
                    //       above as well? Or is it enough just setting it here?
                    // `is_struct` is set to false here. Might be changed to true
                    // if at a later time during index analyzing it sees that
                    // it does a struct index.
                    let root_var = RootVariable::new(var.name.clone(), var_decl_id, false);
                    var.access_instrs = Some((root_var, vec![access_instr]));
                }
            }
        }
    }
}
