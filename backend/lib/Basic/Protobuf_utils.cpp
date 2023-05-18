#include "tlang/Deserializer/Protobuf_utils.h"
#include "frontend.pb.h"
#include <fstream>
#include <iostream>
#include <memory>

namespace tlang {
std::pair<PbErrorCode, Frontend_ir::Program>
deserializeProgram(std::string &filePath) {
  Frontend_ir::Program program;
  std::fstream fileIn(filePath, std::ios::in | std::ios::binary);
  if (!fileIn) {
    return std::make_pair(PbErrorCode::FileNotFound, program);
  }
  if (!program.ParseFromIstream(&fileIn)) {
    return std::make_pair(PbErrorCode::DeserError, program);
  }
  return std::make_pair(PbErrorCode::Nil, program);
}

std::unique_ptr<ProgramIR> protobufToIR(const Frontend_ir::Program &program) {
  return std::unique_ptr<ProgramIR>(new ProgramIR(program));
}
} // namespace tlang
