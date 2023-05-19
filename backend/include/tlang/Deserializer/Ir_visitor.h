#ifndef TLANG_IR_VISITOR_H
#define TLANG_IR_VISITOR_H
#include "Expr_ir.h"
#include "llvm/IR/Value.h"

/* Interface - An Abstract base class with pure virtual functions with no
 * definitions or data members*/
class IRVisitor {
public:
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) = 0;
  virtual ~IRVisitor() = default;
};

#endif
