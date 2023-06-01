#include "tlang/Deserializer/Type_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "llvm/IR/Type.h"
#include <memory>

std::unique_ptr<TypeIR> deserializeType(const Frontend_ir::Type_expr &texpr) {
  switch (texpr.value_case()) {
  case Frontend_ir::Type_expr::kInt32Ty:
    return std::unique_ptr<TypeIR>(new TypeIntIR(texpr.int32ty().rank()));
  default:
    return nullptr;
  }
}

llvm::Type *TypeIntIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
