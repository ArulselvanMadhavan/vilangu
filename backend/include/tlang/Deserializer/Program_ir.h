#ifndef TLANG_DESERIALIZER_PROGRAM_IR_H
#define TLANG_DESERIALIZER_PROGRAM_IR_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Class_ir.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Function_ir.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include <stdlib.h>
#include <vector>
struct ProgramIR {
  std::vector<std::unique_ptr<ClassIR>> classDefns;
  std::vector<std::unique_ptr<StmtIR>> mainExpr;
  std::vector<std::unique_ptr<FunctionIR>> functionDefns;
  ProgramIR(const Frontend_ir::Program &program);
  ProgramIR(){};
};
#endif
