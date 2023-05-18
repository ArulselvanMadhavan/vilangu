#ifndef TLANG_EXPR_IR_H
#define TLANG_EXPR_IR_H

#include "frontend.pb.h"
#include "llvm/IR/Value.h"
#include <stdlib.h>
#include <string>

class IRVisitor;
struct ExprIR {
  virtual ~ExprIR() = default;
  virtual llvm::Value *codegen(IRVisitor &visitor) = 0;
};

struct ExprIntegerIR : public ExprIR {
  int val;
  ExprIntegerIR(const int &i) : val(i) {}
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprFunctionAppIR(const Frontend_ir::Expr::_FunctionApp &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr);
#endif
