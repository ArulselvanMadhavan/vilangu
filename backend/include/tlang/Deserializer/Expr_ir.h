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

struct ExprPrintfIR : public ExprIR {
  std::string formatStr;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprPrintfIR(const Frontend_ir::Expr::_Printf &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

enum Unop { UnopNot, UnopNeg };

struct ExprUnopIR : public ExprIR {
  enum Unop op;
  std::unique_ptr<ExprIR> expr;
  ExprUnopIR(const Frontend_ir::Expr::_Unop &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

enum BinOp { BinOpPlus };

struct ExprBinOpIR : public ExprIR {
  enum BinOp op;
  std::unique_ptr<ExprIR> lexpr;
  std::unique_ptr<ExprIR> rexpr;
  ExprBinOpIR(const Frontend_ir::Expr::_Binop &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct IdentifierIR {
  std::string varName;
  virtual ~IdentifierIR() = default;
  virtual llvm::Value *codegen(IRVisitor &visitor) = 0;
};

struct IdentifierVarIR : public IdentifierIR {
  IdentifierVarIR(const std::string &name);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprVarDeclIR : public ExprIR {
  std::string varName;
  ExprVarDeclIR(const Frontend_ir::Expr::_VarDecl &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprIdentifierIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  ExprIdentifierIR(const Frontend_ir::Identifier &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct ExprAssignIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  std::unique_ptr<ExprIR> assignedExpr;
  ExprAssignIR(const Frontend_ir::Expr::_Assign &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr);
#endif
