#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/Host.h"
#include <iostream>
#include <memory>

IRCodegenVisitor::IRCodegenVisitor() {
  context = std::make_unique<llvm::LLVMContext>();
  builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*context));
  module = std::make_unique<llvm::Module>("Module", *context);
}

void IRCodegenVisitor::codegenMainExpr(
    const std::vector<std::unique_ptr<ExprIR>> &mainExpr) {
  llvm::FunctionType *mainType =
      llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*context),
                              std::vector<llvm::Type *>(), false);
  llvm::Function *main = llvm::Function::Create(
      mainType, llvm::Function::ExternalLinkage, "main", module.get());

  llvm::BasicBlock *mainBasicBlock =
      llvm::BasicBlock::Create(*context, "entry", main);
  builder->SetInsertPoint(mainBasicBlock);
  varEnv.clear();

  for (auto &expr : mainExpr) {
    expr->codegen(*this);
  }
}
void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenExternFunctionDeclarations();
  codegenMainExpr(program.mainExpr);
}
IRCodegenVisitor::~IRCodegenVisitor() {
  std::cout << "Destructor IRCodegenVisitor\n";
}

void IRCodegenVisitor::configureTarget() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }
