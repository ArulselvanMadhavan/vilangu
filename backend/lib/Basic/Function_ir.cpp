#include "tlang/Deserializer/Function_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include <memory>

ParameterIR::ParameterIR(const Frontend_ir::Param &param) {
  paramName = param.param_name();
  paramType = deserializeType(param.param_type());
}

FunctionIR::FunctionIR(const Frontend_ir::FunctionDef &fnDef) {
  functionName = fnDef.name();
  returnType = deserializeType(fnDef.return_t());
  for (auto &p : fnDef.params()) {
    params.push_back(std::unique_ptr<ParameterIR>(new ParameterIR(p)));
  }
  body = deserializeStmt(fnDef.body());
}
