use crate::AnalyzeContext;
use common::visitor::Visitor;

pub struct BlockVisitor<'a> {
    analyze_context: &'a mut AnalyzeContext,
}

impl<'a> BlockVisitor<'a> {}

impl Visitor for BlockVisitor {
}
