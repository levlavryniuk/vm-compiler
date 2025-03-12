use codegen::BytecodeGenerator;
use ir::IrGenerator;
use lexer::Lexer;
use parser::Parser;
use semantic::TypeChecker;
use std::{fs::File, io::Read};
use vm::VM;

mod ast_node;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod semantic;
mod tokens;
mod vm;
fn main() {
    let mut file = File::open("bob").unwrap();

    let mut buf = String::new();

    file.read_to_string(&mut buf).unwrap();

    let mut lexer = Lexer::new(&buf);

    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);

    let stmts = parser.parse().unwrap();

    let mut semantic_checker = TypeChecker::new();

    semantic_checker.check(&stmts).unwrap();

    let mut ir = IrGenerator::new();

    let ir_program = ir.generate(&stmts);

    let mut codegen = BytecodeGenerator::new();

    let bytecode = codegen.generate(&ir_program);

    let mut vm = VM::new(&bytecode);

    dbg!(vm.execute(&bytecode));
}
