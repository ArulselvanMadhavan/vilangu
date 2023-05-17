#ifndef TLANG_PROTOBUF_UTILS_H
#define TLANG_PROTOBUF_UTILS_H
#include "frontend.pb.h"
/* #include "tlang/Deserializer/Program_ir.h" */
#include <string>

namespace tlang {
enum class PbErrorCode { FileNotFound, Nil };
std::pair<PbErrorCode, Frontend_ir::Program>
deserializeProgram(std::string &filePath);
} // namespace tlang
#endif
