#include "tlang/Basic/Version.h"
#include "tlang/Codegen/Ir_codegen_visitor.h"
#include "tlang/Deserializer/Protobuf_utils.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "Hello, I'm Tlang " << tlang::getTlangVersion() << "\n";
  std::string filePath(argv_[1]);
  // Deserialize and handle error
  auto pb_result = tlang::deserializeProgram(filePath);
  if (pb_result.first != tlang::PbErrorCode::Nil) {
    std::cout << "Fail\n";
    return -1;
  }

  // Prog IR
  auto program = pb_result.second;
  auto pir = tlang::protobufToIR(program);

  IRCodegenVisitor codeGen;
  codeGen.codegenProgram(*pir);
  codeGen.configureTarget();
  codeGen.dumpLLVMIR();
  return 0;
}
