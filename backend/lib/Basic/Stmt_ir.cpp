#include "tlang/Deserializer/Stmt_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"

StmtPrintfIR::StmtPrintfIR(const Frontend_ir::Stmt::_Printf &expr) {
  for (int i = 0; i < expr.f_args_size(); i++) {
    arguments.push_back(deserializeExpr(expr.f_args()[i]));
  }
};

StmtVarDeclIR::StmtVarDeclIR(const Frontend_ir::Stmt::_VarDecl &expr) {
  varName = expr.var_id();
  varType = deserializeType(expr.texpr());
}

StmtExprIR::StmtExprIR(const Frontend_ir::Stmt::_ExprStmt &stmt) {
  expr = deserializeExpr(stmt.expr_stmt());
}

StmtIfElseIR::StmtIfElseIR(const Frontend_ir::Stmt::_If_stmt &expr) {
  condExpr = deserializeExpr(expr.eval());
  thenExpr = deserializeStmt(expr.if_stmt());
  elseExpr = deserializeStmt(expr.else_stmt());
}

std::unique_ptr<StmtIR> deserializeStmt(const Frontend_ir::Stmt &stmt) {
  switch (stmt.value_case()) {
  case Frontend_ir::Stmt::kPrintf:
    return std::unique_ptr<StmtIR>(new StmtPrintfIR(stmt.printf()));
  case Frontend_ir::Stmt::kVarDecl:
    return std::unique_ptr<StmtIR>(new StmtVarDeclIR(stmt.vardecl()));
  case Frontend_ir::Stmt::kExprStmt:
    return std::unique_ptr<StmtIR>(new StmtExprIR(stmt.exprstmt()));
  case Frontend_ir::Stmt::kBlock:
    return std::unique_ptr<StmtBlockIR>(new StmtBlockIR(stmt.block()));
  case Frontend_ir::Stmt::kWhile:
    return std::unique_ptr<StmtWhileIR>(new StmtWhileIR(stmt.while_()));
  case Frontend_ir::Stmt::kBreak:
    return std::unique_ptr<StmtBreakIR>(new StmtBreakIR());
  case Frontend_ir::Stmt::kContinue:
    return std::unique_ptr<StmtContinueIR>(new StmtContinueIR());
  case Frontend_ir::Stmt::kIfStmt:
    return std::unique_ptr<StmtIfElseIR>(new StmtIfElseIR(stmt.ifstmt()));
  case Frontend_ir::Stmt::kDelete:
    return std::unique_ptr<StmtDeleteIR>(new StmtDeleteIR(stmt.delete_()));
  default:
    // FIXME
    auto expr = Frontend_ir::Stmt::_Printf::default_instance();
    return std::unique_ptr<StmtIR>(new StmtPrintfIR(expr));
  }
}

StmtBlockIR::StmtBlockIR(const Frontend_ir::Stmt::_Block &stmt) {
  for (int i = 0; i < stmt.stmt_list_size(); i++) {
    auto e = stmt.stmt_list(i);
    stmts.push_back(deserializeStmt(e));
    if (e.has_break_() || e.has_continue_()) {
      break;
    }
  }
}

StmtWhileIR::StmtWhileIR(const Frontend_ir::Stmt::_While &stmt) {
  condExpr = deserializeExpr(stmt.while_cond());
  loopStmt = deserializeStmt(stmt.while_block());
}

StmtDeleteIR::StmtDeleteIR(const Frontend_ir::Stmt::_Delete &stmt) {
  delExpr = deserializeExpr(stmt.del_expr());
}

llvm::Value *StmtPrintfIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtVarDeclIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtExprIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtWhileIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtBlockIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtBreakIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtContinueIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtIfElseIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *StmtDeleteIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
