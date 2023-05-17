#include "tlang/Deserializer/Expr_ir.h"
#include "frontend.pb.h"
#include <memory>
std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr) {
  switch (expr.value_case()) {
  case Frontend_ir::Expr::kInteger:
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(expr.integer()));
  case Frontend_ir::Expr::kFunctionApp:
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(-1));
  default:
    // FIXME
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(-1));
  }
}
