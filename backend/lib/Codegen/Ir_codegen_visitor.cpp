#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/Host.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <iostream>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <memory>

IRCodegenVisitor::IRCodegenVisitor() {
  context = std::make_unique<llvm::LLVMContext>();
  builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*context));
  module = std::make_unique<llvm::Module>("Module", *context);
  loops = new std::stack<LoopInfo *>();
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

  llvm::APInt retVal(32, (uint32_t)0, true);
  builder->CreateRet(llvm::ConstantInt::get(*(context), retVal));
  llvm::verifyFunction(*main);
}
void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenExternFunctionDeclarations();
  codegenMainExpr(program.mainExpr);
  runOptimizingPasses(program.mainExpr);
}
IRCodegenVisitor::~IRCodegenVisitor() {
  // std::cout << "Destructor IRCodegenVisitor\n";
}

void IRCodegenVisitor::configureTarget() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }

void IRCodegenVisitor::runOptimizingPasses(
    const std::vector<std::unique_ptr<ExprIR>> &mainExpr) {
  std::unique_ptr<llvm::legacy::FunctionPassManager> functionPassManager =
      std::make_unique<llvm::legacy::FunctionPassManager>(module.get());
  // Do simple "peephole" optimizations
  // functionPassManager->add(llvm::createInstructionCombiningPass());
  // Simplify the control flow graph (deleting unreachable blocks etc).
  functionPassManager->add(llvm::createCFGSimplificationPass());
  functionPassManager->doInitialization();
  llvm::Function *llvmMainFun = module->getFunction(llvm::StringRef("main"));
  functionPassManager->run(*llvmMainFun);
}
