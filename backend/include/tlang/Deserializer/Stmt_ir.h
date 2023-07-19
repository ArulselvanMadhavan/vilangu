#ifndef TLANG_STMT_IR_H
#define TLANG_STMT_IR_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"
class IRVisitor;

struct StmtIR {
  virtual ~StmtIR() = default;
  virtual llvm::Value *codegen(IRVisitor &visitor) = 0;
};

struct StmtPrintfIR : public StmtIR {
  std::vector<std::unique_ptr<ExprIR>> arguments;
  StmtPrintfIR(const Frontend_ir::Stmt::_Printf &stmt);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtVarDeclIR : public StmtIR {
  std::string varName;
  std::unique_ptr<TypeIR> varType;
  StmtVarDeclIR(const Frontend_ir::Stmt::_VarDecl &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtExprIR : public StmtIR {
  std::unique_ptr<ExprIR> expr;
  StmtExprIR(const Frontend_ir::Stmt::_ExprStmt &stmt);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtWhileIR : public StmtIR {
  std::unique_ptr<ExprIR> condExpr;
  std::unique_ptr<StmtIR> loopStmt;
  StmtWhileIR(const Frontend_ir::Stmt::_While &stmt);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtBlockIR : public StmtIR {
  std::vector<std::unique_ptr<StmtIR>> stmts;
  StmtBlockIR(const Frontend_ir::Stmt::_Block &stmt);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtBreakIR : public StmtIR {
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtContinueIR : public StmtIR {
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtIfElseIR : public StmtIR {
  std::unique_ptr<ExprIR> condExpr;
  std::unique_ptr<StmtIR> thenExpr;
  std::unique_ptr<StmtIR> elseExpr;
  StmtIfElseIR(const Frontend_ir::Stmt::_If_stmt &expr);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};

struct StmtDeleteIR : public StmtIR {
  std::unique_ptr<ExprIR> delExpr;
  StmtDeleteIR(const Frontend_ir::Stmt::_Delete &stmt);
  virtual llvm::Value *codegen(IRVisitor &visitor) override;
};
std::unique_ptr<StmtIR> deserializeStmt(const Frontend_ir::Stmt &stmt);
#endif
