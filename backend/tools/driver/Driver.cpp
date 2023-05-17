#include "tlang/Basic/Version.h"
#include "tlang/Deserializer/Protobuf_utils.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "Hello, I'm Tlang " << tlang::getTlangVersion() << "\n";
  std::string filePath(argv_[1]);
  auto result = tlang::deserializeProgram(filePath);
  switch (result.first) {
  case tlang::PbErrorCode::Nil:
    std::cout << "Success\n";
    break;
  default:
    std::cout << "Fail\n";
    break;
  }
  return 0;
}
