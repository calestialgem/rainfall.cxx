# Setup for C++ 23 standard.
cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED true)
set(CMAKE_CXX_EXTENSIONS off)
project(rainfall CXX)

## Main executable of the Rainfall compiler.
add_executable(${PROJECT_NAME} src/main.cxx)

# Add project source root to include directories.
target_include_directories(${PROJECT_NAME} PRIVATE src)

# Enable all warnings.
target_compile_options(${PROJECT_NAME} PRIVATE
  -Wall
  -Wextra
  -Wpedantic
)
