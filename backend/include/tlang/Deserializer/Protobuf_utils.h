#ifndef TLANG_PROTOBUF_UTILS_H
#define TLANG_PROTOBUF_UTILS_H
#include "frontend.pb.h"
/* #include "tlang/Deserializer/Program_ir.h" */
#include <string>

namespace tlang {
Frontend_ir::Program deserializeProgram(std::string &filePath);
}
#endif
