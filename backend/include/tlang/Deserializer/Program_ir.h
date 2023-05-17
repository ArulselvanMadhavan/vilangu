#ifndef TLANG_DESERIALIZER_PROGRAM_IR_H
#define TLANG_DESERIALIZER_PROGRAM_IR_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include <stdlib.h>
#include <vector>
struct ProgramIR {
  std::vector<std::unique_ptr<ExprIR>> mainExpr;
  ProgramIR(const Frontend_ir::Program &program);
  ProgramIR(){};
};
#endif
