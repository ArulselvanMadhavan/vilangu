#include "tlang/Deserializer/Program_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Class_ir.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Function_ir.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include <iostream>
ProgramIR::ProgramIR(const Frontend_ir::Program &program) {
  for (auto &c : program.classdefs()) {
    classDefns.push_back(std::unique_ptr<ClassIR>(new ClassIR(c)));
  }

  for (auto &f : program.function_defs()) {
    functionDefns.push_back(std::unique_ptr<FunctionIR>(new FunctionIR(f)));
  }

  for (int i = 0; i < program.main_size(); i++) {
    Frontend_ir::Stmt expr = program.main(i);
    mainExpr.push_back(deserializeStmt(expr));
  }
}
