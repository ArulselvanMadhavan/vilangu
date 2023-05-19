#include "tlang/Deserializer/Expr_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include <memory>

ExprFunctionAppIR::ExprFunctionAppIR(
    const Frontend_ir::Expr::_FunctionApp &expr) {
  functionName = expr.name();
  for (int i = 0; i < expr.args_size(); i++) {
    arguments.push_back(deserializeExpr(expr.args()[i]));
  }
};

ExprPrintfIR::ExprPrintfIR(const Frontend_ir::Expr::_Printf &expr) {
  formatStr = expr.format();
  for (int i = 0; i < expr.f_args_size(); i++) {
    arguments.push_back(deserializeExpr(expr.f_args()[i]));
  }
};

std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr) {
  switch (expr.value_case()) {
  case Frontend_ir::Expr::kInteger:
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(expr.integer()));
  case Frontend_ir::Expr::kFunctionApp:
    return std::unique_ptr<ExprIR>(new ExprFunctionAppIR(expr.functionapp()));
  case Frontend_ir::Expr::kPrintf:
    return std::unique_ptr<ExprIR>(new ExprPrintfIR(expr.printf()));
  default:
    // FIXME
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(-1));
  }
}

// Codegen impl

llvm::Value *ExprIntegerIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprFunctionAppIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprPrintfIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
