#ifndef TLANG_FUNCTION_IR_H
#define TLANG_FUNCTION_IR_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include <memory>
#include <string>

struct ParameterIR {
  std::unique_ptr<TypeIR> paramType;
};
struct FunctionIR {
  std::string functionName;
  std::unique_ptr<TypeIR> returnType;
  std::vector<std::unique_ptr<ParameterIR>> params;
  std::vector<std::unique_ptr<ExprIR>> bodyExpr;
};
#endif
