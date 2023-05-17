#include "tlang/Deserializer/Protobuf_utils.h"
#include "tlang/Deserializer/frontend.pb.h"
#include <iostream>
namespace tlang {
Frontend_ir::Program deserializeProgram(std::string &filePath) {
  Frontend_ir::Program program;
  std::cout << "Deser called\n" << filePath;
  return program;
}
} // namespace tlang
