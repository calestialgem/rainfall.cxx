#include "lib.cxx"

#include <iostream>
#include <span>
#include <string_view>

/// Entry into the compiler.
int main(int const argc, char const* const* const argv) noexcept
{
  // Test arguments.
  for (std::string_view argument: std::span(argv, argc))
    std::cout << argument << std::endl;

  // Test the project setup.
  std::cout << rf::provide_message() << std::endl;
}
