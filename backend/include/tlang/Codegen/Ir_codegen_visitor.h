#ifndef TLANG_IR_CODEGEN_VISITOR_H
#define TLANG_IR_CODEGEN_VISITOR_H
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "tlang/Deserializer/Program_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <memory>
#include <stack>
#include <stdlib.h>
#include <vector>

struct LoopInfo {
  llvm::BasicBlock *loopBegin;
  llvm::BasicBlock *loopEnd;
  LoopInfo(llvm::BasicBlock *loopBB, llvm::BasicBlock *loopEndBB)
      : loopBegin(loopBB), loopEnd(loopEndBB){};
  virtual ~LoopInfo() = default;
};

class IRCodegenVisitor : public IRVisitor {
protected:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::stack<LoopInfo *> *loops;
  std::map<std::string, llvm::AllocaInst *> varEnv;

public:
  IRCodegenVisitor();
  ~IRCodegenVisitor();
  void codegenExternFunctionDeclarations();
  void codegenProgram(const ProgramIR &program);
  void codegenMainExpr(const std::vector<std::unique_ptr<ExprIR>> &mainExpr);
  void configureTarget();
  void dumpLLVMIR();
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) override;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) override;
  virtual llvm::Value *codegen(const ExprPrintfIR &expr) override;
  virtual llvm::Value *codegen(const ExprUnopIR &expr) override;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprVarDeclIR &expr) override;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) override;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) override;
  virtual llvm::Value *codegen(const IdentifierVarIR &expr) override;
  virtual llvm::Value *codegen(const ExprBlockIR &expr) override;
  virtual llvm::Value *codegen(const ExprIfElseIR &expr) override;
  virtual llvm::Value *codegen(const ExprWhileIR &expr) override;
  virtual llvm::Value *codegen(const ExprBreakIR &expr) override;
  virtual llvm::Type *codegen(const TypeIntIR &texpr) override;
  void
  runOptimizingPasses(const std::vector<std::unique_ptr<ExprIR>> &mainExpr);
};
#endif
