cmake_minimum_required(VERSION 3.12)
project(rainfall CXX)

add_executable(${PROJECT_NAME} src/main.cxx)
set_target_properties(${PROJECT_NAME} PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED true
  CXX_EXTENSIONS off
)
target_compile_options(${PROJECT_NAME} PRIVATE
  -Wall
  -Wextra
  -Wpedantic
)
