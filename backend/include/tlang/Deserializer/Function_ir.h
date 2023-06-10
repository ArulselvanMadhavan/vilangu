#ifndef TLANG_FUNCTION_IR_H
#define TLANG_FUNCTION_IR_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include <memory>
#include <string>

struct ParameterIR {
  std::unique_ptr<TypeIR> paramType;
  std::string paramName;
  ParameterIR(const Frontend_ir::Param &param);
};

struct FunctionIR {
  std::string functionName;
  std::unique_ptr<TypeIR> returnType;
  std::vector<std::unique_ptr<ParameterIR>> params;
  std::unique_ptr<StmtIR> body;

  FunctionIR(const Frontend_ir::FunctionDef &fnDef);
};
#endif
