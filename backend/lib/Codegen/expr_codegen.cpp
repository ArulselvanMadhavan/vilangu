#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>
#include <string>
#include <vector>

llvm::Value *IRCodegenVisitor::codegen(const ExprIntegerIR &expr) {
  return llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)),
                                      expr.val);
}

llvm::Value *IRCodegenVisitor::codegen(const ExprFunctionAppIR &expr) {
  llvm::Function *calleeFun =
      module->getFunction(llvm::StringRef(expr.functionName));

  if (calleeFun == nullptr) {
    std::cout << "Callfun is null\n";
    // return error code and handle it
    return nullptr;
  }
  llvm::FunctionType *calleeFunTy = calleeFun->getFunctionType();
  std::vector<llvm::Value *> argVals;
  int arglen = 1;
  for (int i = 0; i < arglen; i++) {
    llvm::Value *argVal = expr.arguments[i]->codegen(*this);
    if (argVal == nullptr) {
      return nullptr;
    }
    llvm::Type *paramTy = calleeFunTy->getParamType(i);
    llvm::Value *bitCastArgVal = builder->CreateBitCast(argVal, paramTy);
    argVals.push_back(bitCastArgVal);
  }
  return builder->CreateCall(calleeFun, argVals);
}

llvm::Value *IRCodegenVisitor::codegen(const ExprPrintfIR &expr) {
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  printfArgs.push_back(builder->CreateGlobalStringPtr(expr.formatStr));
  for (auto &arg : expr.arguments) {
    llvm::Value *argVal = arg->codegen(*this);
    if (argVal == nullptr) {
      llvm::outs() << "printf argval is null";
      return nullptr;
    }
    printfArgs.push_back(argVal);
  }
  return builder->CreateCall(printf, printfArgs);
}

llvm::Value *IRCodegenVisitor::codegen(const ExprUnopIR &expr) {
  llvm::Value *exprVal = expr.expr->codegen(*this);
  if (exprVal == nullptr) {
    llvm::outs() << "unop expr is null";
    return nullptr;
  }
  switch (expr.op) {
  case UnopNot:
    return builder->CreateNot(exprVal, "not");
  case UnopNeg:
    return builder->CreateNeg(exprVal, "neg");
  }
}

llvm::Value *IRCodegenVisitor::codegen(const ExprBinOpIR &expr) {
  llvm::Value *lexpr = expr.lexpr->codegen(*this);
  llvm::Value *rexpr = expr.rexpr->codegen(*this);
  if (lexpr == nullptr || rexpr == nullptr) {
    llvm::outs() << "bin op operand is null";
    return nullptr;
  }
  switch (expr.op) {
  case BinOpPlus:
    return builder->CreateAdd(lexpr, rexpr, "add");
  }
}

llvm::Value *IRCodegenVisitor::codegen(const ExprVarDeclIR &expr) {
  // TODO: Get type of variable
  llvm::Value *boundVal =
    llvm::ConstantInt::getSigned(expr.varType->codegen(*this), 0);
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> TmpBuilder(&(parentFunction->getEntryBlock()),
                               parentFunction->getEntryBlock().begin());
  llvm::AllocaInst *var = TmpBuilder.CreateAlloca(boundVal->getType(), nullptr,
                                                  llvm::Twine(expr.varName));
  varEnv[expr.varName] = var;
  builder->CreateStore(boundVal, var);
  return boundVal;
}

llvm::Value *IRCodegenVisitor::codegen(const ExprAssignIR &expr) {
  llvm::Value *assignedVal = expr.assignedExpr->codegen(*this);
  llvm::Value *id = expr.identifier->codegen(*this);
  if (id == nullptr) {
    llvm::outs() << "Trying to assign to a null id";
    return nullptr;
  }
  builder->CreateStore(assignedVal, id);
  return assignedVal;
}

llvm::Value *IRCodegenVisitor::codegen(const IdentifierVarIR &var) {
  llvm::Value *val = varEnv[var.varName];
  if (val == nullptr) {
    llvm::outs() << "Var not found: " + var.varName;
  }
  return val;
}

llvm::Value *IRCodegenVisitor::codegen(const ExprIdentifierIR &expr) {
  llvm::Value *id = expr.identifier->codegen(*this);
  if (id == nullptr) {
    llvm::outs() << "Identifier not found: " + expr.identifier->varName;
    return nullptr;
  }

  llvm::Type *idType = id->getType()->isPointerTy() ? id->getType()->getPointerElementType() : id->getType();
  llvm::Value *idVal = builder->CreateLoad(idType, id);
  if (idVal == nullptr) {
    llvm::outs() << "Identifier not loaded: " + expr.identifier->varName;
    return nullptr;
  }
  return idVal;
}
