#include "tlang/Codegen/Ir_codegen_visitor.h"
#include <llvm-14/llvm/IR/DerivedTypes.h>
#include <llvm-14/llvm/IR/Type.h>

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  module->getOrInsertFunction(
      "out", llvm::FunctionType::get(
                 llvm::IntegerType::getInt32Ty(*context),
                 llvm::Type::getInt8Ty(*context)->getPointerTo(), true));
}
