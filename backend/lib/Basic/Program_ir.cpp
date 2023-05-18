#include "tlang/Deserializer/Program_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include <iostream>
ProgramIR::ProgramIR(const Frontend_ir::Program &program) {
  for (int i = 0; i < program.main_size(); i++) {
    Frontend_ir::Expr expr = program.main(i);
    mainExpr.push_back(deserializeExpr(expr));
  }
}
