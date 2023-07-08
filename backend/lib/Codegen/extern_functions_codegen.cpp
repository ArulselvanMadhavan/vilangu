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

void IRCodegenVisitor::addGlobalVarStr(std::string varName,
                                       llvm::StringRef content) {
  auto contentPtr = llvm::ConstantDataArray::getString(*context, content);
  module->getOrInsertGlobal(varName, contentPtr->getType());
  llvm::GlobalVariable *varGV = module->getNamedGlobal(varName);
  varGV->setLinkage(llvm::GlobalValue::PrivateLinkage);
  varGV->setConstant(true);
  varGV->setAlignment(llvm::Align());
  varGV->setInitializer(contentPtr);
}

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  llvm::StringRef initVal = "%d\n";
  addGlobalVarStr(getPrintIntFormatVar(), initVal);
  llvm::StringRef castErr = "Narrow cast err: %s is not a type %s\n";
  addGlobalVarStr(getCastErrFormatVar(), castErr);
  llvm::StringRef oob = "Line: %d | Array out of bounds exception at line. "
                        "Index:%d | Length:%d\n";
  addGlobalVarStr(getOutOfBoundsFormatVar(), oob);
  llvm::StringRef nlen =
      "Line: %d | Array len cannot be negative. Length attempted:%d\n";
  addGlobalVarStr(getNegativeLenFormatVar(), nlen);
  llvm::StringRef nullDeref = "Line: %d | Null dereference\n";
  addGlobalVarStr(getNullDerefFormatVar(), nullDeref);

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
