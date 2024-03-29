#include "tlang/Codegen/Ir_codegen_visitor.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/Casting.h>

void IRCodegenVisitor::codegenClasses(
    const std::vector<std::unique_ptr<ClassIR>> &classes) {
  for (auto &currClass : classes) {
    llvm::StructType::create(*context, llvm::StringRef(currClass->className));
    llvm::StructType::create(
        *context, llvm::StringRef(getVtableTypeName(currClass->className)));
  }
  for (auto &currClass : classes) {
    llvm::StructType *classType = llvm::StructType::getTypeByName(
        *context, llvm::StringRef(currClass->className));
    std::vector<llvm::Type *> bodyTypes;
    // vtable type
    llvm::Type *vTableTy = llvm::StructType::getTypeByName(
        *context, getVtableTypeName(currClass->className));
    bodyTypes.push_back(vTableTy->getPointerTo());
    // inits.push_back(llvm::Constant::getNullValue(vTableTy->getPointerTo()));
    for (auto &field : currClass->fields) {
      llvm::Type *fieldType = field->codegen(*this);
      bodyTypes.push_back(fieldType);
      // inits.push_back(llvm::Constant::getNullValue(fieldType));
    }
    classType->setBody(llvm::ArrayRef<llvm::Type *>(bodyTypes));
  }
}

void IRCodegenVisitor::codegenVTables(
    const std::vector<std::unique_ptr<ClassIR>> &classes) {
  // fill in struct bodies
  for (auto &currClass : classes) {
    std::string vTableTyName = getVtableTypeName(currClass->className);
    llvm::StructType *vTableTy = llvm::StructType::getTypeByName(
        *context, llvm::StringRef(vTableTyName));
    std::vector<llvm::Type *> bodyTypes;
    std::vector<llvm::Constant *> inits;
    // Base class vtable
    std::string baseClassVTableTyName =
        getVtableTypeName(currClass->baseClassName);
    llvm::StructType *baseClassVtableTy = llvm::StructType::getTypeByName(
        *context, llvm::StringRef(baseClassVTableTyName));
    bodyTypes.push_back(baseClassVtableTy->getPointerTo());
    std::string baseClassVTableName = getVtableName(currClass->baseClassName);
    llvm::GlobalVariable *baseClassVtable =
        module->getNamedGlobal(baseClassVTableName);
    if (baseClassVtable == nullptr) {
      inits.push_back(
          llvm::Constant::getNullValue(baseClassVtableTy->getPointerTo()));
    } else {
      inits.push_back(baseClassVtable);
    }

    llvm::StringRef classNameVal = currClass->className;
    std::string classNameGlobalVarName = currClass->className + "_class_name";
    addGlobalVarStr(classNameGlobalVarName, classNameVal);

    llvm::GlobalVariable *classNameVar =
        module->getNamedGlobal(classNameGlobalVarName);
    llvm::Value *zeroIdx =
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0);
    llvm::Value *classNameBegin = builder->CreateInBoundsGEP(
        classNameVar->getType()->getContainedType(0), classNameVar,
        llvm::ArrayRef<llvm::Value *>{zeroIdx, zeroIdx});
    bodyTypes.push_back(llvm::Type::getInt8PtrTy(*context));
    if (auto *temp = llvm::dyn_cast<llvm::Constant>(classNameBegin)) {
      inits.push_back(temp);
    }

    for (auto &methodName : currClass->vtable) {
      llvm::Function *method = module->getFunction(llvm::StringRef(methodName));
      inits.push_back(method);
      bodyTypes.push_back(method->getType());
    }

    // Complete vtable type defs
    vTableTy->setBody(llvm::ArrayRef<llvm::Type *>(bodyTypes));
    std::string vTableName = getVtableName(currClass->className);
    module->getOrInsertGlobal(vTableName, vTableTy);
    llvm::GlobalVariable *vTable = module->getNamedGlobal(vTableName);

    vTable->setInitializer(llvm::ConstantStruct::get(
        vTableTy, llvm::ArrayRef<llvm::Constant *>(inits)));
  }

  // Gen Is a function
  // llvm::Type *objTypePtr =
  //     llvm::StructType::getTypeByName(*context, "Object")->getPointerTo();
  // llvm::Type *namePtr = llvm::Type::getInt8PtrTy(*context, 0);
  // llvm::Type *retType = llvm::Type::getInt1Ty(*context);
  // llvm::FunctionType *isAFunType = llvm::FunctionType::get(
  //     retType, llvm::ArrayRef<llvm::Type *>({objTypePtr, namePtr}), false);
  // llvm::Function *isAFun = llvm::Function::Create(
  //     isAFunType, llvm::Function::ExternalLinkage, "Object_IsA",
  //     module.get());
  // llvm::BasicBlock *initBB = llvm::BasicBlock::Create(*context, "init",
  // isAFun); llvm::Function::Create();
}
