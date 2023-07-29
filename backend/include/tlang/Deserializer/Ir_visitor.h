#ifndef TLANG_IR_VISITOR_H
#define TLANG_IR_VISITOR_H
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"
/* Interface - An Abstract base class with pure virtual functions with no
 * definitions or data members*/
class IRVisitor {
public:
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprNullIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtPrintfIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprUnopIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtVarDeclIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprCastIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprMethodCallIR &expr) = 0;
  virtual llvm::Value *codegen(const SimpleVarIR &expr) = 0;
  virtual llvm::Value *codegen(const SubscriptVarIR &expr) = 0;
  virtual llvm::Value *codegen(const FieldVarIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtBlockIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtIfElseIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtWhileIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtDeleteIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtFreeIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtRetIR &expr) = 0;  
  virtual llvm::Value *codegen(const StmtBreakIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtContinueIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprEmptyIR &expr) = 0;
  virtual llvm::Value *codegen(const StmtExprIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprArrayMakeIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprClassMakeIR &expr) = 0;
  virtual llvm::Type *codegen(const TypeIntIR &texpr) = 0;
  virtual llvm::Type *codegen(const TypeClassIR &texpr) = 0;
  virtual llvm::Type *codegen(const TypePointerIR &texpr) = 0;
  virtual llvm::Type *codegen(const TypeBoolIR &texpr) = 0;
  virtual llvm::Type *codegen(const TypeInt8IR &texpr) = 0;
  virtual llvm::Type *codegen(const TypeVoidIR &texpr) = 0;
  virtual llvm::Value *codegen(const ExprVarIR &expr) = 0;
  virtual llvm::Value *codegen(const LoadVarIR &expr) = 0;
  virtual ~IRVisitor() = default;
};

#endif
