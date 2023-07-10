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

std::string IRCodegenVisitor::getPrintIntFormatVar() {
  return "printIntFormat";
}
std::string IRCodegenVisitor::getCastErrFormatVar() { return "castErrFormat"; }
std::string IRCodegenVisitor::getOutOfBoundsFormatVar() {
  return "outOfBoundsFormat";
}
std::string IRCodegenVisitor::getNegativeLenFormatVar() {
  return "negativeLenFormat";
}

std::string IRCodegenVisitor::getNullDerefFormatVar() {
  return "nullDerefFormat";
}

std::string IRCodegenVisitor::getDivByZeroFormatVar() {
  return "divByZeroFormat";
}

void IRCodegenVisitor::codegenMainExpr(
    const std::vector<std::unique_ptr<StmtIR>> &mainExpr) {
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
  codegenClasses(program.classDefns);
  codegenFunctionProtos(program.functionDefns);
  codegenVTables(program.classDefns);
  codegenFunctionDefns(program.functionDefns);
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
    const std::vector<std::unique_ptr<StmtIR>> &mainExpr) {
  std::unique_ptr<llvm::legacy::FunctionPassManager> functionPassManager =
      std::make_unique<llvm::legacy::FunctionPassManager>(module.get());
  // Do simple "peephole" optimizations
  functionPassManager->add(llvm::createInstructionCombiningPass());
  // Simplify the control flow graph (deleting unreachable blocks etc).
  // functionPassManager->add(llvm::createCFGSimplificationPass());
  functionPassManager->doInitialization();
  llvm::Function *llvmMainFun = module->getFunction(llvm::StringRef("main"));
  functionPassManager->run(*llvmMainFun);
}

std::string IRCodegenVisitor::getTypeNameAsString(llvm::Type *t) {
  std::string type_str;
  llvm::raw_string_ostream rso(type_str);
  t->print(rso);
  return type_str;
}

std::string IRCodegenVisitor::getVtableName(std::string className) {
  return className + "_Vtable";
}

std::string IRCodegenVisitor::getVtableTypeName(std::string className) {
  return getVtableName(className) + "_type";
}

void IRCodegenVisitor::runtimeError(std::string formatStr,
                                    llvm::ArrayRef<llvm::Value *> args) {
  llvm::Value *zeroIdx =
      llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  llvm::GlobalVariable *printVar = module->getNamedGlobal(formatStr);
  llvm::Value *printVarBegin = builder->CreateInBoundsGEP(
      printVar->getType()->getContainedType(0), printVar,
      llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});
  printfArgs.insert(printfArgs.begin(), printVarBegin);
  printfArgs.insert(printfArgs.end(), std::make_move_iterator(args.begin()),
                    std::make_move_iterator(args.end()));
  builder->CreateCall(printf, printfArgs);
  llvm::Function *exit = module->getFunction("exit");
  builder->CreateCall(
      exit, llvm::ArrayRef<llvm::Value *>{llvm::ConstantInt::getSigned(
                llvm::Type::getInt32Ty(*context), -1)});
}
