find_package(Protobuf REQUIRED)
protobuf_generate_cpp(GENERATED_SRC GENERATED_HEADER
../../../frontend/frontend.proto)

add_tlang_library(tlangBasic Version.cpp Protobuf_utils.cpp
Program_ir.cpp Expr_ir.cpp ${GENERATED_SRC})

add_custom_command(TARGET tlangBasic POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy
                   ${GENERATED_HEADER}
                   ${CMAKE_SOURCE_DIR}/include/tlang/Deserializer/frontend.pb.h)

target_link_libraries(tlangBasic PRIVATE protobuf::libprotobuf)
# This seems extraneous
target_include_directories(tlangBasic PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
