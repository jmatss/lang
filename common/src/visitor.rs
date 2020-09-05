use crate::token::ast::AstToken;

pub trait Visitor {
    /*
        TOP LEVEL
    */
    fn visit_block(ast_token: AstToken);
    fn visit_expr();
    fn visit_stmt();

    /*
        BLOCKS
    */
    fn visit_default_block();
    fn visit_func();
    fn visit_struct();
    fn visit_enum();
    fn visit_interface();
    fn visit_impl();
    fn visit_anon();
    fn visit_if();
    fn visit_if_case();
    fn visit_match();
    fn visist_match_case();
    fn visit_for();
    fn visit_while();
    fn visit_test();

    /*
        STATEMENTS
    */
    fn visit_return();
    fn visit_yield();
    fn visit_break();
    fn visit_contine();
    fn visit_use();
    fn visit_package();
    fn visit_defer();
    fn visit_defer_exec();
    fn visit_assignment();
    fn visit_var_decl();
    fn visit_extern_decl();
    fn visit_modifier();

    /*
        EXPRESSIONS
    */
    fn visist_lit();
    fn visist_type();
    fn visit_var();
    fn visist_func_call();
    fn visit_struct_init();
    fn visist_array_init();
    fn visit_op();

    /*
        OPERATIONS
    */
    fn visit_bin_op();
    fn visit_un_op();

    /*
        BINARY OPERATIONS
    */
    fn visit_bin_in();
    fn visit_bin_is();
    fn visit_bin_as();
    fn visit_bin_of();
    fn visit_bin_range();
    fn visit_bin_range_incl();
    fn visit_bin_dot();
    fn visit_bin_eq();
    fn visit_bin_neq();
    fn visit_bin_lt();
    fn visit_bin_gt();
    fn visit_bin_lte();
    fn visit_bin_gte();
    fn visit_bin_add();
    fn visit_bin_sub();
    fn visit_bin_mul();
    fn visit_bin_div();
    fn visit_bin_mod();
    fn visit_bin_pow();
    fn visit_bin_bit_and();
    fn visit_bin_bit_or();
    fn visit_bin_bit_xor();
    fn visit_bin_shl();
    fn visit_bin_shr();
    fn visit_bin_bool_and();
    fn visit_bin_bool_or();

    /*
        UNARY OPERATIONS
    */
    fn visit_un_inc();
    fn visit_un_dec();
    fn visit_un_deref();
    fn visit_un_address();
    fn visit_un_pos();
    fn visit_un_neg();
    fn visit_un_array_access();
    fn visit_un_bit_compliment();
    fn visit_un_bool_not();
}
