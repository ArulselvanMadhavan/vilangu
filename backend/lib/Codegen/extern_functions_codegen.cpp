#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/raw_ostream.h>

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  llvm::StringRef initVal = "%d\n";
  auto initVal2 = llvm::ConstantDataArray::getString(*context, initVal);
  module->getOrInsertGlobal(getPrintIntFormatVar(), initVal2->getType());
  llvm::GlobalVariable *gVar = module->getNamedGlobal(getPrintIntFormatVar());
  gVar->setLinkage(llvm::GlobalValue::PrivateLinkage);
  gVar->setConstant(true);
  gVar->setAlignment(llvm::Align());
  gVar->setInitializer(initVal2);

  llvm::StringRef castErr = "Narrow cast err: %s is not a type %s\n";
  auto castErrPtr = llvm::ConstantDataArray::getString(*context, castErr);
  module->getOrInsertGlobal(getCastErrFormatVar(), castErrPtr->getType());
  llvm::GlobalVariable *castErrGV =
      module->getNamedGlobal(getCastErrFormatVar());
  castErrGV->setLinkage(llvm::GlobalValue::PrivateLinkage);
  castErrGV->setConstant(true);
  castErrGV->setAlignment(llvm::Align());
  castErrGV->setInitializer(castErrPtr);

  llvm::StringRef oob = "Line: %d | Array out of bounds exception at line. "
                        "Index:%d | Length:%d\n";
  auto oobPtr = llvm::ConstantDataArray::getString(*context, oob);
  module->getOrInsertGlobal(getOutOfBoundsFormatVar(), oobPtr->getType());
  llvm::GlobalVariable *oobGV =
      module->getNamedGlobal(getOutOfBoundsFormatVar());
  oobGV->setLinkage(llvm::GlobalValue::PrivateLinkage);
  oobGV->setConstant(true);
  oobGV->setAlignment(llvm::Align());
  oobGV->setInitializer(oobPtr);

  llvm::StringRef nlen =
      "Line: %d | Array len cannot be negative. Length attempted:%d\n";
  auto nlenPtr = llvm::ConstantDataArray::getString(*context, nlen);
  module->getOrInsertGlobal(getNegativeLenFormatVar(), nlenPtr->getType());
  llvm::GlobalVariable *nlenGV =
      module->getNamedGlobal(getNegativeLenFormatVar());
  nlenGV->setLinkage(llvm::GlobalValue::PrivateLinkage);
  nlenGV->setConstant(true);
  nlenGV->setAlignment(llvm::Align());
  nlenGV->setInitializer(nlenPtr);

  module->getOrInsertFunction(
      "printf", llvm::FunctionType::get(
                    llvm::IntegerType::getInt32Ty(*context),
                    llvm::Type::getInt8Ty(*context)->getPointerTo(), true));
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();
  llvm::Type *voidTy = llvm::Type::getVoidTy(*context);
  module->getOrInsertFunction(
      "malloc", llvm::FunctionType::get(
                    voidPtrTy, llvm::IntegerType::getInt32Ty(*context), false

                    ));
  auto params =
      llvm::ArrayRef<llvm::Type *>{llvm::IntegerType::getInt32Ty(*context),
                                   llvm::IntegerType::getInt32Ty(*context)};
  module->getOrInsertFunction(
      "calloc", llvm::FunctionType::get(voidPtrTy, params, false));
  module->getOrInsertFunction(
      "free", llvm::FunctionType::get(voidTy, voidPtrTy, false));

  module->getOrInsertFunction(
      "exit", llvm::FunctionType::get(
                  voidTy, llvm::IntegerType::getInt32Ty(*context), false));
}
