#ifndef TLANG_IR_VISITOR_H
#define TLANG_IR_VISITOR_H
#include "Expr_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"

/* Interface - An Abstract base class with pure virtual functions with no
 * definitions or data members*/
class IRVisitor {
public:
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprPrintfIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprUnopIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprVarDeclIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) = 0;
  virtual llvm::Value *codegen(const IdentifierVarIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBlockIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprIfElseIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprWhileIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBreakIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprContinueIR &expr) = 0;
  virtual llvm::Type *codegen(const TypeIntIR &texpr) = 0;
  virtual ~IRVisitor() = default;
};

#endif
