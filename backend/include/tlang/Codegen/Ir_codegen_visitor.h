#ifndef TLANG_IR_CODEGEN_VISITOR_H
#define TLANG_IR_CODEGEN_VISITOR_H
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "tlang/Deserializer/Program_ir.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <memory>
#include <stdlib.h>
#include <vector>

class IRCodegenVisitor : public IRVisitor {
protected:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

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
};
#endif
