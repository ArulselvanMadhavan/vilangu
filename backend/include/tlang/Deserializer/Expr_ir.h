#ifndef TLANG_EXPR_IR_H
#define TLANG_EXPR_IR_H

#include "frontend.pb.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"
#include <memory>
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

enum Unop { UnopNot, UnopNeg };

struct ExprUnopIR : public ExprIR {
  enum Unop op;
  std::unique_ptr<ExprIR> expr;
  ExprUnopIR(const Frontend_ir::Expr::_Unop &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

enum BinOp {
  BinOpPlus,
  BinOpEquals,
  BinOpLessThan,
  BinOpGreaterThan,
  BinOpMult,
  BinOpDivide,
  BinOpSubtract
};

struct ExprBinOpIR : public ExprIR {
  enum BinOp op;
  std::unique_ptr<ExprIR> lexpr;
  std::unique_ptr<ExprIR> rexpr;
  ExprBinOpIR(const Frontend_ir::Expr::_Binop &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct VarIR {
  virtual ~VarIR() = default;
  virtual llvm::Value *codegen(IRVisitor &visitor) = 0;
};

struct SimpleVarIR : public VarIR {
  std::string varName;
  SimpleVarIR(const std::string &name);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct SubscriptVarIR : public VarIR {
  std::unique_ptr<VarIR> baseVar;
  std::unique_ptr<ExprIR> expr;
  std::unique_ptr<VarIR> lenVar;
  int lineNo;
  SubscriptVarIR(const Frontend_ir::Var::_Subscript &var);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct FieldVarIR : public VarIR {
  std::unique_ptr<ExprIR> baseExpr;
  int field_index;
  FieldVarIR(const Frontend_ir::Var::_Field &var);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct LoadVarIR : public VarIR {
  std::unique_ptr<VarIR> baseVar;
  LoadVarIR(const Frontend_ir::Var::_Load &var);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprVarIR : public ExprIR {
  std::unique_ptr<VarIR> var;
  ExprVarIR(const Frontend_ir::Var &var);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};
struct ExprIdentifierIR : public ExprIR {
  std::unique_ptr<VarIR> var;
  ExprIdentifierIR(const Frontend_ir::Identifier &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprAssignIR : public ExprIR {
  std::unique_ptr<VarIR> identifier;
  std::unique_ptr<ExprIR> assignedExpr;
  ExprAssignIR(const Frontend_ir::Expr::_Assign &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprEmptyIR : public ExprIR {
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprArrayMakeIR : public ExprIR {
  std::vector<std::unique_ptr<ExprIR>> creationExprs;
  std::unique_ptr<TypeIR> varType;
  ExprArrayMakeIR(const Frontend_ir::Expr::_ArrayCreation &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprNullIR : public ExprIR {
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

enum CastType { Identity, Wide, Narrow };

struct ExprCastIR : public ExprIR {
  std::unique_ptr<TypeIR> castTo;
  std::unique_ptr<ExprIR> expr;
  enum CastType castType;
  ExprCastIR(const Frontend_ir::Expr::_CastExpr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr);
std::unique_ptr<VarIR> deserializeVar(const Frontend_ir::Var &var);
#endif
