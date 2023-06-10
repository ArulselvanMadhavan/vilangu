#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "llvm/IR/Function.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Verifier.h>
#include <vector>

llvm::FunctionType *
IRCodegenVisitor::codegenFunctionType(const FunctionIR &function) {
  std::vector<llvm::Type *> paramTypes;
  for (auto &param : function.params) {
    paramTypes.push_back(param->paramType->codegen(*this));
  }
  llvm::Type *returnType = function.returnType->codegen(*this);
  return llvm::FunctionType::get(returnType, paramTypes, false /* isVarArgs */
  );
}
void IRCodegenVisitor::codegenFunctionProtos(
    const std::vector<std::unique_ptr<FunctionIR>> &fnDefs) {
  for (auto &fn : fnDefs) {
    llvm::FunctionType *fnType = codegenFunctionType(*fn);
    llvm::Function::Create(fnType, llvm::Function::ExternalLinkage,
                           fn->functionName, module.get());
  }
}

void IRCodegenVisitor::codegenFunctionDefn(const FunctionIR &function) {
  llvm::Function *llvmFun =
      module->getFunction(llvm::StringRef(function.functionName));
  llvm::BasicBlock *entryBasicBlock =
      llvm::BasicBlock::Create(*context, "entry", llvmFun);
  builder->SetInsertPoint(entryBasicBlock);

  // initialise var env with function params
  std::map<std::string, llvm::AllocaInst *> tempEnv(varEnv);
  varEnv.clear();

  for (auto &param : llvmFun->args()) {
    int paramNo = param.getArgNo();
    std::string paramName = function.params[paramNo]->paramName;
    llvm::Type *paramType = llvmFun->getFunctionType()->getParamType(paramNo);
    varEnv[paramName] =
        builder->CreateAlloca(paramType, nullptr, llvm::Twine(paramName));
    builder->CreateStore(&param, varEnv[paramName]);
  }

  // gen code for body of function
  llvm::Value *returnValue; // this is the value of the last expr in the body
  returnValue = function.body->codegen(*this);

  // create a return instruction from last expression
  if (llvmFun->getReturnType()->isVoidTy()) {
    builder->CreateRetVoid();
  } else {
    builder->CreateRet(returnValue);
  }

  llvm::verifyFunction(*llvmFun);

  varEnv.clear();
  varEnv = tempEnv;
  tempEnv.clear();
}

void IRCodegenVisitor::codegenFunctionDefns(
    const std::vector<std::unique_ptr<FunctionIR>> &functions) {
  for (auto &function : functions) {
    codegenFunctionDefn(*function);
  }
}
