find_package(Protobuf REQUIRED)
protobuf_generate_cpp(GENERATED_SRC GENERATED_HEADER ../../../frontend/frontend.proto)
add_tlang_library(tlangBasic Version.cpp ${GENERATED_SRC} ${GENERATED_HEADER})
target_link_libraries(tlangBasic PRIVATE ${Protobuf_LIBRARIES})
target_include_directories(tlangBasic PRIVATE ${Protobuf_INCLUDE_DIRS}
${CMAKE_CURRENT_BINARY_DIR})
message(STATUS "PB ${Protobuf_LIBRARIES} ${GENERATED_SRC}")
