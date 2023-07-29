#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"
#include <llvm-14/llvm/ADT/Twine.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/GlobalVariable.h>
#include <llvm-14/llvm/IR/Type.h>
#include <llvm-14/llvm/Support/raw_ostream.h>
#include <llvm/IR/Function.h>

llvm::Value *IRCodegenVisitor::codegen(const StmtVarDeclIR &stmt) {
  // TODO: Get type of variable
  llvm::BasicBlock *currentBB = builder->GetInsertBlock();
  llvm::Type *varType = stmt.varType->codegen(*this);
  llvm::Value *boundVal = llvm::Constant::getNullValue(varType);
  builder->SetInsertPoint(currentBB);
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> TmpBuilder(&(parentFunction->getEntryBlock()),
                               parentFunction->getEntryBlock().begin());
  llvm::AllocaInst *var =
      TmpBuilder.CreateAlloca(varType, nullptr, llvm::Twine(stmt.varName));
  varEnv[stmt.varName] = var; // Always stores a pointer; i32arr**
  builder->CreateStore(boundVal, var);
  return boundVal;
}

llvm::Value *IRCodegenVisitor::codegen(const StmtPrintfIR &expr) {
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  llvm::GlobalVariable *gVar = module->getNamedGlobal(getPrintIntFormatVar());
  llvm::Value *zeroIdx =
      llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
  llvm::Value *gVarBegin = builder->CreateInBoundsGEP(
      gVar->getType()->getContainedType(0), gVar,
      llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});

  printfArgs.push_back(gVarBegin);
  for (auto &arg : expr.arguments) {
    llvm::Value *assignedVal = arg->codegen(*this);
    if (assignedVal == nullptr) {
      llvm::outs() << "printf argval is null";
      return nullptr;
    }
    printfArgs.push_back(assignedVal);
  }
  return builder->CreateCall(printf, printfArgs);
}

llvm::Value *IRCodegenVisitor::codegen(const StmtExprIR &stmt) {
  return stmt.expr->codegen(*this);
}

llvm::Value *IRCodegenVisitor::codegen(const StmtBlockIR &expr) {
  llvm::Value *lastExprVal;
  for (auto &e : expr.stmts) {
    lastExprVal = (e->codegen(*this));
  }
  return lastExprVal;
}

llvm::Value *IRCodegenVisitor::codegen(const StmtContinueIR &expr) {
  LoopInfo *currentLoop = loops->top();
  currentLoop->hasContinue = true;
  builder->CreateBr(currentLoop->loopCond);
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
}

llvm::Value *IRCodegenVisitor::codegen(const StmtBreakIR &expr) {
  LoopInfo *currentLoop = loops->top();
  currentLoop->hasBreak = true;
  builder->CreateBr(currentLoop->loopEnd);
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
}

llvm::Value *IRCodegenVisitor::codegen(const StmtWhileIR &expr) {
  llvm::BasicBlock *currentBB = builder->GetInsertBlock();
  llvm::Function *parentFunc = currentBB->getParent();

  // BB
  llvm::BasicBlock *loopCondBB = llvm::BasicBlock::Create(*context, "loopcond");
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*context, "loop");
  llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*context, "loopend");

  auto currentLoop = new LoopInfo(loopCondBB, loopEndBB);
  loops->push(currentLoop);
  builder->CreateBr(loopCondBB);

  // Cond BB
  parentFunc->getBasicBlockList().push_back(loopCondBB);
  builder->SetInsertPoint(loopCondBB);
  llvm::Value *condValue = expr.condExpr->codegen(*this);
  if (condValue == nullptr) {
    llvm::outs() << "Null condition expr for while statement";
    return nullptr;
  }
  loopCondBB = builder->GetInsertBlock();
  builder->CreateCondBr(condValue, loopBB, loopEndBB);

  // loopBB
  parentFunc->getBasicBlockList().push_back(loopBB);
  builder->SetInsertPoint(loopBB);
  // codegen
  expr.loopStmt->codegen(*this);
  loopBB = builder->GetInsertBlock();
  if (loopBB->getTerminator() == nullptr) {
    // Possibly we may have to remove instructions after the terminator
    builder->CreateBr(loopCondBB);
  }
  // loopEndBB
  parentFunc->getBasicBlockList().push_back(loopEndBB);
  builder->SetInsertPoint(loopEndBB);

  loops->pop();
  delete currentLoop;
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
}

llvm::Value *IRCodegenVisitor::codegen(const StmtIfElseIR &expr) {
  llvm::Value *condValue = expr.condExpr->codegen(*this);
  if (condValue == nullptr) {
    llvm::outs() << "Null cond expression in if-else statement.";
  }
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(*context, "then", parentFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*context, "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*context, "ifcont");
  builder->CreateCondBr(condValue, thenBB, elseBB);

  builder->SetInsertPoint(thenBB);
  llvm::Value *thenVal = expr.thenExpr->codegen(*this);
  if (thenVal == nullptr) {
    llvm::outs() << "Then block evaluates to null in if-else statement.";
    return nullptr;
  }
  thenBB = builder->GetInsertBlock();
  if (thenBB->getTerminator() == nullptr) {
    // Block could have inserted branch inst already. Ex: break, continue
    // Insert a branch only if one hasn’t been inserted already.
    builder->CreateBr(mergeBB);
  }

  parentFunction->getBasicBlockList().push_back(elseBB);
  builder->SetInsertPoint(elseBB);
  llvm::Value *elseVal = expr.elseExpr->codegen(*this);
  if (elseVal == nullptr) {
    llvm::outs() << "Else block evaluates to null in if-else statement.";
    return nullptr;
  }
  elseBB = builder->GetInsertBlock();
  if (elseBB->getTerminator() == nullptr) {
    // Block could have inserted branch inst already. Ex: break, continue
    // Insert a branch only if one hasn’t been inserted already.
    builder->CreateBr(mergeBB);
  }

  // merge block
  parentFunction->getBasicBlockList().push_back(mergeBB);
  builder->SetInsertPoint(mergeBB);

  // if either is void or their types don't match (which indicates one of them
  // returned the null value for void, then don't construct a phi node)
  if (thenVal->getType() == llvm::Type::getVoidTy(*context) ||
      elseVal->getType() == llvm::Type::getVoidTy(*context) ||
      (thenVal->getType() != elseVal->getType())) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
  }
  llvm::PHINode *phiNode = builder->CreatePHI(thenVal->getType(), 2, "iftmp");
  phiNode->addIncoming(thenVal, thenBB);
  phiNode->addIncoming(elseVal, elseBB);
  return phiNode;
}

llvm::Value *IRCodegenVisitor::codegen(const StmtDeleteIR &stmt) {
  // auto exprVal = stmt.delExpr->codegen(*this);
  // llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();
  // auto voidPtr = builder->CreateBitCast(exprVal, voidPtrTy);
  // llvm::Function *freeFunc = module->getFunction("free");
  // return builder->CreateCall(freeFunc, llvm::ArrayRef<llvm::Value
  // *>{voidPtr});
  return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), -1);
}

llvm::Value *IRCodegenVisitor::codegen(const StmtFreeIR &stmt) {
  auto exprVal = stmt.freeExpr->codegen(*this);
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();
  auto voidPtr = builder->CreateBitCast(exprVal, voidPtrTy);
  llvm::Function *freeFunc = module->getFunction("free");
  return builder->CreateCall(freeFunc, llvm::ArrayRef<llvm::Value *>{voidPtr});
}

llvm::Value *IRCodegenVisitor::codegen(const StmtRetIR &stmt){
  auto retExpr = stmt.retExpr->codegen(*this);
  return retExpr;
}
