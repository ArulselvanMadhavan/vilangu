set(LLVM_LINK_COMPONENTS Support)

add_tlang_tool(tlang Driver.cpp)

target_link_libraries(tlang PRIVATE tlangBasic)
target_link_libraries(tlang PRIVATE tlangCodegen)
