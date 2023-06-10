#ifndef TLANG_TYPE_IR_H
#define TLANG_TYPE_IR_H

#include "frontend.pb.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include <cstdint>
#include <memory>

class IRVisitor;

struct TypeIR {
  virtual ~TypeIR() = default;
  virtual llvm::Type *codegen(IRVisitor &visitor) = 0;
};

std::unique_ptr<TypeIR> deserializeType(const Frontend_ir::Type_expr &texpr);

struct TypeIntIR : public TypeIR {
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};

struct TypeClassIR : public TypeIR {
  std::string className;
  TypeClassIR(const std::string &name) : className(name) {}
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};

struct TypePointerIR : public TypeIR {
  std::unique_ptr<TypeIR> data;
  TypePointerIR(const Frontend_ir::Type_expr::_Pointer &texpr) {
    data = deserializeType(texpr.data());
  }
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};

struct TypeVoidIR : public TypeIR {
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};

struct TypeBoolIR : public TypeIR {
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};

struct TypeInt8IR : public TypeIR {
  virtual llvm::Type *codegen(IRVisitor &visitor) override;
};
#endif
