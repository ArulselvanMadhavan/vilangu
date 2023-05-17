#ifndef TLANG_BASIC_VERSION_H
#define TLANG_BASIC_VERSION_H

#include "tlang/Basic/Version.inc"
#include <string>

namespace tlang {
std::string getTlangVersion();
void deserializePrograms(std::string &filePath);
} // namespace tlang
#endif
