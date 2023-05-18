#ifndef TLANG_IR_VISITOR_H
#define TLANG_IR_VISITOR_H
#include "Expr_ir.h"
#include "llvm/IR/Value.h"

class IRVisitor {
public:
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) = 0;
};

#endif
