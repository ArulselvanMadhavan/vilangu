#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "tlang/Deserializer/frontend.pb.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <string>
#include <vector>

llvm::Value *IRCodegenVisitor::codegen(const ExprIntegerIR &expr) {
  return llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)),
                                      expr.val);
}

llvm::Value *IRCodegenVisitor::codegen(const ExprNullIR &expr) {
  // FIXME: Remove hardcoded object
  llvm::Type *o = llvm::StructType::getTypeByName(*context, "Object");
  return llvm::ConstantPointerNull::getNullValue(o->getPointerTo());
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
  case BinOpEquals:
    if (lexpr->getType()->isPointerTy()) {
      lexpr = builder->CreatePtrToInt(lexpr, llvm::Type::getInt64Ty(*context));
    }
    if (rexpr->getType()->isPointerTy()) {
      rexpr = builder->CreatePtrToInt(rexpr, llvm::Type::getInt64Ty(*context));
    }
    return builder->CreateICmpEQ(lexpr, rexpr, "equal");
  case BinOpLessThan:
    return builder->CreateICmpSLT(lexpr, rexpr, "slt");
  case BinOpGreaterThan:
    return builder->CreateICmpSGT(lexpr, rexpr, "sgt");
  case BinOpMult:
    return builder->CreateMul(lexpr, rexpr, "mul");
  case BinOpDivide:
    return builder->CreateExactSDiv(lexpr, rexpr, "sdiv");
  case BinOpSubtract:
    return builder->CreateSub(lexpr, rexpr, "sub");
  }
}

llvm::Value *IRCodegenVisitor::codegen(const ExprAssignIR &expr) {
  llvm::Value *rhs = expr.assignedExpr->codegen(*this);
  llvm::Value *lhs = expr.identifier->codegen(*this); // Pointer val
  if (lhs == nullptr) {
    llvm::outs() << "Trying to assign to a null id";
    return nullptr;
  }
  // lhs - i32* or struct_type* or struct_type**
  llvm::Value *lhsVal = lhs;
  llvm::Type *lhsType = lhs->getType();
  llvm::Type *rhsType = rhs->getType();
  llvm::Value *rhsVal = rhs;
  if (lhsType == rhsType) {
    // lhsType->print(llvm::outs());
    // rhsType->print(llvm::outs());
    rhsVal = builder->CreateLoad(rhsType->getContainedType(0), rhs,
                                 llvm::Twine("loadRefTypeBeforeSave"));
  }
  // struct_type* happens when it’s a field access or subscript_access - we
  // need to save things at this location; rhs should be struct_type
  // struct_type** happens when you directly; rhs should be struct_type*
  // load a reference type variable from varEnv

  // lhs is a var - load the address
  return builder->CreateStore(rhsVal, lhsVal);
}

llvm::Value *IRCodegenVisitor::codegen(const LoadVarIR &var) {
  llvm::Value *baseVal = var.baseVar->codegen(*this);
  if (baseVal == nullptr) {
    llvm::outs() << "BaseVal is null";
    return nullptr;
  }
  llvm::Type *baseType = baseVal->getType();
  if (baseType->isPointerTy() == false) {
    llvm::outs() << "LoadVar: baseType is not a pointer";
    return nullptr;
  }
  return builder->CreateLoad(baseType->getContainedType(0), baseVal,
                             llvm::Twine("loadVar"));
}

llvm::Value *IRCodegenVisitor::codegen(const SimpleVarIR &var) {
  llvm::Value *id = varEnv[var.varName];
  if (id == nullptr) {
    llvm::outs() << "Var not found: " + var.varName;
  }
  return id;
}

llvm::Value *IRCodegenVisitor::codegen(const SubscriptVarIR &var) {
  llvm::Value *subscriptExpr = var.expr->codegen(*this);
  if (subscriptExpr == nullptr) {
    llvm::outs() << "Subscript expr evaluated to null";
    return nullptr;
  }
  llvm::Value *loadedVal;
  if (subscriptExpr->getType()->isPointerTy()) {
    loadedVal = builder->CreateLoad(
        subscriptExpr->getType()->getContainedType(0), subscriptExpr);
  } else {
    loadedVal = subscriptExpr;
  }

  // (elemTy*)* where elemTy is stty or prim
  llvm::Value *baseArrPtrPtr = var.baseVar->codegen(*this);
  if (baseArrPtrPtr->getType()->isPointerTy() == false) {
    llvm::outs() << "subscript var expected a pointer type but got";
    baseArrPtrPtr->getType()->print(llvm::outs());
    return nullptr;
  }
  llvm::Value *baseArrPtr =
      builder->CreateLoad(baseArrPtrPtr->getType()->getContainedType(0),
                          baseArrPtrPtr, llvm::Twine("baseArrPtr"));
  if (baseArrPtr->getType()->isPointerTy() == false) {
    llvm::outs() << "subscript var expected a pointer type but got";
    baseArrPtrPtr->getType()->print(llvm::outs());
    return nullptr;
  }
  llvm::Type *baseType = baseArrPtr->getType()->getContainedType(0);
  // Bounds check
  // 1. Get length of the array
  // 2. If loadedVal < length; phi normal
  // 3. else call print with err_string and exit
  llvm::Value *lenPtr = var.lenVar->codegen(*this); // i32*
  llvm::Value *lenVal =
      builder->CreateLoad(lenPtr->getType()->getContainedType(0), lenPtr);
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(*context, "boundsCheckThen", parentFunction);
  llvm::BasicBlock *elseBB =
      llvm::BasicBlock::Create(*context, "boundsCheckElse");
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(*context, "boundsCheckIfcont");
  llvm::Value *condValue = builder->CreateICmpSLT(loadedVal, lenVal);
  builder->CreateCondBr(condValue, thenBB, elseBB);

  parentFunction->getBasicBlockList().push_back(thenBB);
  builder->SetInsertPoint(thenBB);
  llvm::Value *thenVal =
      builder->CreateInBoundsGEP(baseType, baseArrPtr, loadedVal);
  builder->CreateBr(mergeBB);

  // Print err in the else block
  parentFunction->getBasicBlockList().push_back(elseBB);
  builder->SetInsertPoint(elseBB);
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  llvm::GlobalVariable *printVar =
      module->getNamedGlobal(getOutOfBoundsFormatVar());
  llvm::Value *zeroIdx =
      llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
  llvm::Value *printVarBegin = builder->CreateInBoundsGEP(
      printVar->getType()->getContainedType(0), printVar,
      llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});
  printfArgs.push_back(printVarBegin);
  printfArgs.push_back(llvm::ConstantInt::getSigned(
      llvm::Type::getInt32Ty(*context), var.lineNo));
  printfArgs.push_back(loadedVal);
  printfArgs.push_back(lenVal);
  builder->CreateCall(printf, printfArgs);

  llvm::Function *exit = module->getFunction("exit");
  builder->CreateCall(
      exit, llvm::ArrayRef<llvm::Value *>{llvm::ConstantInt::getSigned(
                llvm::Type::getInt32Ty(*context), -1)});
  // call exit
  builder->CreateBr(mergeBB);

  // merge block
  parentFunction->getBasicBlockList().push_back(mergeBB);
  builder->SetInsertPoint(mergeBB);
  return thenVal;

  // // // (stty*) or (prim*)

  // llvm::Type *baseType = baseArrPtrPtr->getType()->getContainedType(0);

  // if (baseArrPtr->getType()->isPointerTy() == false) {
  //   baseType = baseArrPtr->getType();
  //   baseArrPtr = baseArrPtrPtr;
  // } else {
  //   baseType = baseArrPtr->getType()->getContainedType(0);
  // }
  // if (baseType->isStructTy() == false) {
  //   // elty*
  //   llvm::Type *arrayType = baseType;
  //   llvm::Value *elemAddr =
  //       builder->CreateInBoundsGEP(arrayType, baseArrPtr, loadedVal);
  //   return elemAddr;
  // } else {
  //   // structty -> already indexed via field_index; this.data - stty**
  //   // // structty -> not indexed - a - stty**
  //   // llvm::Value *arrayPtrPtr =
  //   //     builder->CreateStructGEP(baseType, baseArrPtr, 0); // elemType**
  //   // llvm::Type *arrayPtrType =
  //   //     arrayPtrPtr->getType()->getContainedType(0); // elemType*
  //   // llvm::Value *arrayPtr =
  //   //     builder->CreateLoad(arrayPtrType, arrayPtrPtr); // elemType*

  //   llvm::Type *arrayType = baseType->getContainedType(0); //
  //   llvm::Value *elemAddr =
  //       builder->CreateInBoundsGEP(arrayType, arrayPtr, loadedVal); //
  //       elemType*
  //   return elemAddr;
  // }
}

llvm::Value *IRCodegenVisitor::codegen(const FieldVarIR &var) {
  llvm::Value *baseValPtr = var.baseExpr->codegen(*this);
  if (baseValPtr->getType()->isPointerTy()) {
    return builder->CreateStructGEP(baseValPtr->getType()->getContainedType(0),
                                    baseValPtr,
                                    var.field_index); // fieldVal*
  } else {
    llvm::outs() << "FieldVar Expected a pointer type but received";
    baseValPtr->getType()->print(llvm::outs());
    return nullptr;
  }
}

llvm::Value *IRCodegenVisitor::codegen(const ExprIdentifierIR &expr) {
  llvm::Value *id = expr.var->codegen(*this);
  // llvm::Type *idType = id->getType()->isPointerTy()
  //                          ? id->getType()->getPointerElementType()
  //                          : id->getType();
  // llvm::Type *idType = id->getType();
  // if (idType->isPointerTy() == false) {
  //   llvm::outs() << "Identifier expects a pointer type";
  //   return nullptr;
  // }
  // llvm::Value *idVal = builder->CreateLoad(idType->getContainedType(0), id);
  // return idVal;
  return id;
  // return expr.var->codegen(*this);
}

llvm::Value *IRCodegenVisitor::codegen(const ExprEmptyIR &expr) {
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
}

llvm::Value *IRCodegenVisitor::codegen(const ExprArrayMakeIR &expr) {
  auto &creationExpr = expr.creationExprs[0];
  llvm::Value *size = creationExpr->codegen(*this);
  if (size->getType()->isIntegerTy() == false) {
    llvm::outs() << "array size is not an integer";
    return nullptr;
  }
  // Negative int check
  // llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  // llvm::BasicBlock *thenBB =
  //     llvm::BasicBlock::Create(*context, "negativeLenThen", parentFunction);
  // llvm::BasicBlock *mergeBB =
  //     llvm::BasicBlock::Create(*context, "negativeLenCont");
  // llvm::Value *zeroIdx =
  //     llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
  // llvm::Value *condValue = builder->CreateICmpSLT(zeroIdx, size);
  // builder->CreateCondBr(condValue, thenBB, mergeBB);

  // parentFunction->getBasicBlockList().push_back(thenBB);
  // builder->SetInsertPoint(thenBB);
  // runtimeError(getNegativeLenFormatVar(), llvm::ArrayRef<llvm::Value *>{});
  // builder->CreateBr(mergeBB);

  // parentFunction->getBasicBlockList().push_back(mergeBB);
  // builder->SetInsertPoint(mergeBB);

  llvm::Type *resultTypePtr = expr.varType->codegen(*this); // i32arr*
  llvm::Type *resultType;                                   // i32arr
  if (resultTypePtr->isPointerTy()) {
    resultType = resultTypePtr->getContainedType(0);
  } else {
    llvm::outs() << "ArrayCreation resultType is not a pointer";
    return nullptr;
  }
  llvm::AllocaInst *newArrayResultPtr =
      builder->CreateAlloca(resultType, nullptr, llvm::Twine("newArrayResult"));
  llvm::Value *elemPtrPtr =
      builder->CreateStructGEP(resultType, newArrayResultPtr, 1); // elem**
  llvm::Type *elemPtrType = elemPtrPtr->getType()->getContainedType(0);
  llvm::Type *elemType = elemPtrType->getContainedType(0); // i32
  llvm::Type *int32Type = llvm::Type::getInt32Ty(*context);
  llvm::Value *sizeOfPtr = builder->CreateGEP(
      elemType,
      llvm::ConstantPointerNull::get(llvm::PointerType::get(elemType, 0)),
      llvm::ConstantInt::getSigned(int32Type, 1),
      llvm::Twine("sizeOf")); // sizeof
  llvm::Value *sizeOf = builder->CreatePtrToInt(sizeOfPtr, int32Type);

  llvm::Function *calloc = module->getFunction("calloc");
  auto callocParams = llvm::ArrayRef<llvm::Value *>{size, sizeOf};
  llvm::CallInst *callocRes = builder->CreateCall(calloc, callocParams);
  llvm::Value *callocHead = builder->CreateBitCast(callocRes, elemPtrType);

  // constructor
  llvm::Twine resultTypeStr = resultType->getStructName() + "_Constructor";
  llvm::Function *constructorFunc = module->getFunction(resultTypeStr.str());
  llvm::Twine arrVTableName = resultType->getStructName() + "_Vtable";
  llvm::GlobalVariable *arrVtable = module->getNamedGlobal(arrVTableName.str());
  builder->CreateCall(constructorFunc,
                      llvm::ArrayRef<llvm::Value *>{
                          newArrayResultPtr, arrVtable, callocHead, size});
  return newArrayResultPtr;
}

llvm::Value *IRCodegenVisitor::codegen(const ExprVarIR &expr) {
  llvm::Value *varResult = expr.var->codegen(*this);
  llvm::Type *idType = varResult->getType();
  if (idType->isPointerTy() == false) {
    llvm::outs() << "ExprVar expects a pointer type";
    return nullptr;
  }

  if (idType->getContainedType(0)->isStructTy()) {
    // StructTy are returned by reference
    return varResult;
  } else {
    llvm::Value *idVal =
        builder->CreateLoad(idType->getContainedType(0), varResult);
    return idVal;
  }
}

llvm::Value *IRCodegenVisitor::codegen(const ExprCastIR &expr) {
  switch (expr.castType) {
  case Wide: {
    llvm::Type *castToTy = expr.castTo->codegen(*this);
    llvm::Value *castExprVal = expr.expr->codegen(*this);
    return builder->CreateBitCast(castExprVal, castToTy);
  }
  case Narrow: {
    llvm::Value *castExprVal =
        expr.expr->codegen(*this); // assume this is object type
    llvm::Type *exprType = castExprVal->getType()->getContainedType(0);
    llvm::Twine exprTypeName = exprType->getStructName();
    std::string exprTypeVtable = getVtableName(exprTypeName.str());
    llvm::GlobalVariable *vtableVar = module->getNamedGlobal(exprTypeVtable);

    // Object_IsA call
    llvm::Function *object_IsA_func =
        module->getFunction(exprTypeName.str() + "_IsA");
    std::vector<llvm::Value *> funArgs;
    // Assume cast is always done on ref type
    llvm::Type *castToTyPtr = expr.castTo->codegen(*this);
    llvm::Type *castToTy = castToTyPtr->getContainedType(0);
    llvm::Twine castToClassName = castToTy->getStructName() + "_class_name";
    llvm::Value *zeroIdx =
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
    llvm::GlobalVariable *gVar = module->getNamedGlobal(castToClassName.str());
    llvm::Value *gVarBegin = builder->CreateInBoundsGEP(
        gVar->getType()->getContainedType(0), gVar,
        llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});

    funArgs.push_back(castExprVal);
    funArgs.push_back(vtableVar);
    funArgs.push_back(gVarBegin);
    // If(Object_IsA(object, name))
    llvm::Value *condValue = builder->CreateCall(object_IsA_func, funArgs);

    llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB =
        llvm::BasicBlock::Create(*context, "then", parentFunction);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*context, "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*context, "ifcont");
    builder->CreateCondBr(condValue, thenBB, elseBB);

    builder->SetInsertPoint(thenBB);
    llvm::Value *thenVal = builder->CreateBitCast(castExprVal, castToTyPtr);
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
    // Print err in the else block
    parentFunction->getBasicBlockList().push_back(elseBB);
    builder->SetInsertPoint(elseBB);
    llvm::Function *printf = module->getFunction("printf");
    std::vector<llvm::Value *> printfArgs;
    llvm::GlobalVariable *printVar =
        module->getNamedGlobal(getCastErrFormatVar());
    llvm::Value *printVarBegin = builder->CreateInBoundsGEP(
        printVar->getType()->getContainedType(0), printVar,
        llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});

    auto errorArg1 =
        llvm::ConstantDataArray::getString(*context, exprTypeName.str());
    auto errArgLoc1 = builder->CreateAlloca(errorArg1->getType(), nullptr);
    builder->CreateStore(errorArg1, errArgLoc1);
    llvm::Value *errArg1Begin = builder->CreateInBoundsGEP(
        errorArg1->getType(), errArgLoc1,
        llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});

    auto errorArg2 = llvm::ConstantDataArray::getString(
        *context, getTypeNameAsString(castToTyPtr));
    auto errArgLoc2 = builder->CreateAlloca(errorArg2->getType(), nullptr);
    builder->CreateStore(errorArg2, errArgLoc2);
    llvm::Value *errArg2Begin = builder->CreateInBoundsGEP(
        errorArg2->getType(), errArgLoc2,
        llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});

    printfArgs.push_back(printVarBegin);
    printfArgs.push_back(errArg1Begin);
    printfArgs.push_back(errArg2Begin);

    builder->CreateCall(printf, printfArgs);
    llvm::Function *exit = module->getFunction("exit");
    builder->CreateCall(
        exit, llvm::ArrayRef<llvm::Value *>{llvm::ConstantInt::getSigned(
                  llvm::Type::getInt32Ty(*context), -1)});
    elseBB = builder->GetInsertBlock();
    if (elseBB->getTerminator() == nullptr) {
      // Block could have inserted branch inst already. Ex: break, continue
      // Insert a branch only if one hasn’t been inserted already.
      builder->CreateBr(mergeBB);
    }

    // // merge block
    parentFunction->getBasicBlockList().push_back(mergeBB);
    builder->SetInsertPoint(mergeBB);

    // if either is void or their types don't match (which indicates one of
    // them
    // returned the null value for void, then don't construct a phi node)
    if (thenVal->getType() == llvm::Type::getVoidTy(*context)
        //     // elseVal->getType() == llvm::Type::getVoidTy(*context) ||
    ) {
      return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
    }
    llvm::PHINode *phiNode = builder->CreatePHI(thenVal->getType(), 1, "iftmp");
    phiNode->addIncoming(thenVal, thenBB);
    // phiNode->addIncoming(elseVal, elseBB);
    return phiNode;
  }
  case Identity:
    return expr.expr->codegen(*this);
  }
}
