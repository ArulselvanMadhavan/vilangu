#ifndef TLANG_TYPE_IR_H
#define TLANG_TYPE_IR_H

#include "frontend.pb.h"
#include "llvm/IR/Type.h"

class IRVisitor;

struct TypeIR {
  virtual ~TypeIR() = default;
  virtual llvm::Type *codegen(IRVisitor &visitor) = 0;
};

std::unique_ptr<TypeIR> deserializeType(const Frontend_ir::Type_expr &texpr);

struct TypeIntIR : public TypeIR {
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};
#endif
