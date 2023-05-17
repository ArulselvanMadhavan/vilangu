#include "tlang/Basic/Version.h"
#include "frontend.pb.h"
std::string tlang::getTlangVersion() {
  GOOGLE_PROTOBUF_VERIFY_VERSION;
  return TLANG_VERSION_STRING;
}
