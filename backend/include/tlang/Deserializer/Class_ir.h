#ifndef TLANG_CLASS_IR_H
#define TLANG_CLASS_IR_H

#include "frontend.pb.h"
#include "tlang/Deserializer/Type_ir.h"
#include <string>
#include <vector>

struct ClassIR {
  std::string className;
  std::vector<std::unique_ptr<TypeIR>> fields;
  std::string baseClassName;
  ClassIR(const Frontend_ir::ClassDef &cdef);
};
#endif
