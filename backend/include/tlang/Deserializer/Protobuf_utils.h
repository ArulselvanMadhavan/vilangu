#ifndef TLANG_PROTOBUF_UTILS_H
#define TLANG_PROTOBUF_UTILS_H
#include "frontend.pb.h"
#include "tlang/Deserializer/Program_ir.h"
#include <memory>
#include <string>

namespace tlang {
enum class PbErrorCode { FileNotFound, DeserError, Nil };
std::pair<PbErrorCode, Frontend_ir::Program>
deserializeProgram(std::string &filePath);
std::unique_ptr<ProgramIR> protobufToIR(const Frontend_ir::Program &program);
} // namespace tlang
#endif
