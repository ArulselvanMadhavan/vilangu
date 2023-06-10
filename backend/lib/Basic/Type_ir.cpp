#include "tlang/Deserializer/Type_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "llvm/IR/Type.h"
#include <llvm/IR/DerivedTypes.h>
#include <memory>

std::unique_ptr<TypeIR> deserializeType(const Frontend_ir::Type_expr &texpr) {
  switch (texpr.value_case()) {
  case Frontend_ir::Type_expr::kInt32:
    return std::unique_ptr<TypeIntIR>(new TypeIntIR());
  case Frontend_ir::Type_expr::kClass:
    return std::unique_ptr<TypeClassIR>(new TypeClassIR(texpr.class_().name()));
  case Frontend_ir::Type_expr::kPointer:
    return std::unique_ptr<TypePointerIR>(new TypePointerIR(texpr.pointer()));
  case Frontend_ir::Type_expr::kVoid:
    return std::unique_ptr<TypeVoidIR>(new TypeVoidIR());
  case Frontend_ir::Type_expr::kBool:
    return std::unique_ptr<TypeBoolIR>(new TypeBoolIR());
  case Frontend_ir::Type_expr::kInt8:
    return std::unique_ptr<TypeInt8IR>(new TypeInt8IR());
  default:
    llvm::outs() << "unrecognized type";
    return nullptr;
  }
}

llvm::Type *TypeIntIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Type *TypeClassIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Type *TypePointerIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Type *TypeVoidIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Type *TypeBoolIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Type *TypeInt8IR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
