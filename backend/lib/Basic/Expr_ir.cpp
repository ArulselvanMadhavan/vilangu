#include "tlang/Deserializer/Expr_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <string>

enum Unop deserializeUnop(const Frontend_ir::Un_op &op) {
  switch (op.value_case()) {
  case Frontend_ir::Un_op::kNeg:
    return Unop::UnopNeg;
  case Frontend_ir::Un_op::kNot:
    return Unop::UnopNot;
  default:
    llvm::outs() << "Unmatched unary op\n";
    return Unop::UnopNot;
  }
}

enum BinOp deserializeBinOp(const Frontend_ir::Bin_op &op) {
  switch (op.value_case()) {
  case Frontend_ir::Bin_op::kPlus:
    return BinOp::BinOpPlus;
  default:
    llvm::outs() << "Unmatched unary op\n";
    return BinOp::BinOpPlus;
  }
}

ExprUnopIR::ExprUnopIR(const Frontend_ir::Expr::_Unop &unopExpr) {
  op = deserializeUnop(unopExpr.op());
  expr = deserializeExpr(unopExpr.uexpr());
};

ExprBinOpIR::ExprBinOpIR(const Frontend_ir::Expr::_Binop &binopExpr) {
  op = deserializeBinOp(binopExpr.bin_op());
  lexpr = deserializeExpr(binopExpr.lexpr());
  rexpr = deserializeExpr(binopExpr.rexpr());
};

ExprFunctionAppIR::ExprFunctionAppIR(
    const Frontend_ir::Expr::_FunctionApp &expr) {
  functionName = expr.name();
  for (int i = 0; i < expr.args_size(); i++) {
    arguments.push_back(deserializeExpr(expr.args()[i]));
  }
};

ExprPrintfIR::ExprPrintfIR(const Frontend_ir::Expr::_Printf &expr) {
  formatStr = expr.format();
  formatStr = formatStr.append("\n"); // Add new line to format string
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
  case Frontend_ir::Expr::kUnop:
    return std::unique_ptr<ExprIR>(new ExprUnopIR(expr.unop()));
  case Frontend_ir::Expr::kBinop:
    return std::unique_ptr<ExprIR>(new ExprBinOpIR(expr.binop()));
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

llvm::Value *ExprUnopIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprBinOpIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
