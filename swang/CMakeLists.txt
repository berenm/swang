set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(swang
  swang.cpp
  )

target_link_libraries(swang
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  )
