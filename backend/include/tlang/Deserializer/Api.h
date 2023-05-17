#ifndef TLANG_DESERIALIZER_API_H
#define TLANG_DESERIALIZER_API_H
#include "frontend.pb.h"
#include <stdlib.h>
#include <vector>
struct ProgramIR {
  std::vector<std::unique_ptr<ExprIR>> mainExpr;
  ProgramIR(const Frontend_ir::Program &program);
  ProgramIR() {}
}
#endif
