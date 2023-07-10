#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Type.h"
#include <llvm-14/llvm/ADT/ArrayRef.h>
#include <llvm-14/llvm/IR/Verifier.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <string>

llvm::Type *IRCodegenVisitor::codegen(const TypeIntIR &texpr) {
  llvm::Type *int32Type = llvm::Type::getInt32Ty(*context);
  //   return getArrayTypeFromRank(int32Type, texpr.rank);
  return int32Type;
}

llvm::Type *IRCodegenVisitor::codegen(const TypeClassIR &texpr) {
  llvm::Type *classType = llvm::StructType::getTypeByName(
      *context, llvm::StringRef(texpr.className));
  return classType;
}

llvm::Type *IRCodegenVisitor::codegen(const TypePointerIR &texpr) {
  llvm::Type *curType = texpr.data->codegen(*this);
  llvm::Type *curTypePtr = llvm::PointerType::get(curType, 0);
  return curTypePtr;
}

llvm::Type *IRCodegenVisitor::codegen(const TypeVoidIR &texpr) {
  llvm::Type *voidTy = llvm::Type::getVoidTy(*context);
  return voidTy;
}

llvm::Type *IRCodegenVisitor::codegen(const TypeBoolIR &texpr) {
  return llvm::Type::getInt1Ty(*context);
}

llvm::Type *IRCodegenVisitor::codegen(const TypeInt8IR &texpr) {
  return llvm::Type::getInt8Ty(*context);
}
