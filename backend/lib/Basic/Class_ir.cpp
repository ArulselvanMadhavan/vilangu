#include "tlang/Deserializer/Class_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Type_ir.h"

ClassIR ::ClassIR(const Frontend_ir::ClassDef &classdef) {
  className = classdef.name();
  baseClassName = classdef.baseclassname();
  for (auto s : classdef.vtable()){
    vtable.push_back(s);
  }
  for (auto &f : classdef.fields()) {
    fields.push_back(std::unique_ptr<TypeIR>(deserializeType(f)));
  }
}
