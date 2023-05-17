#include "tlang/Deserializer/Protobuf_utils.h"
#include "tlang/Deserializer/frontend.pb.h"
#include <fstream>
#include <iostream>

namespace tlang {
std::pair<PbErrorCode, Frontend_ir::Program>
deserializeProgram(std::string &filePath) {
  Frontend_ir::Program program;
  std::fstream fileIn(filePath, std::ios::in | std::ios::binary);
  if (!fileIn) {
    return std::make_pair(PbErrorCode::FileNotFound, program);
  }
  // if (!program.ParseFromIstream(&fileIn)) {
  //   throw DeserialiseProtobufException("Protobuf not deserialised from
  //   file.");
  // }
  return std::make_pair(PbErrorCode::Nil, program);
}
} // namespace tlang
