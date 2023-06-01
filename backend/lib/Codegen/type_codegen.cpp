#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Type.h"

llvm::Type *IRCodegenVisitor::codegen(const TypeIntIR &texpr) {
  return llvm::Type::getInt32Ty(*context);
}
