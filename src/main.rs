use codegen::BytecodeGenerator;
pub mod log;
use ir::IrGenerator;
use lexer::Lexer;
use log::set_log_level;
use parser::Parser;
use semantic::{Context, TypeChecker};
use std::{fs::File, io::Read};
use vm::VM;

pub use crate::log::LogLevel;
pub use crate::log::{debug, error, info, trace, warn};
mod ast_node;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod semantic;
mod tokens;
mod vm;
//
fn main() {
    set_log_level(LogLevel::Debug);
    let mut file = File::open("bob.deez").unwrap();

    let mut buf = String::new();

    file.read_to_string(&mut buf).unwrap();

    let mut lexer = Lexer::new(&buf);

    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let stmts = parser.parse().expect("Parser sais:");

    let mut semantic_checker = TypeChecker::new();

    let mut ctx = Context::new();
    semantic_checker
        .check(&mut ctx, &stmts)
        .expect("Type checker said:");

    let mut ir = IrGenerator::new();

    let ir_program = ir.generate(&stmts);

    let mut codegen = BytecodeGenerator::new();

    let bytecode = codegen.generate(&ir_program);

    let mut vm = VM::new(&bytecode);

    let _ = vm.execute(&bytecode);
    ()
}
