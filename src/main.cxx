#include "utility.hxx"

#include <stdio.h>
#include <stdlib.h>

int main(int const argument_count, char const* const* const arguments) {
  for (int i = 0; i < argument_count; i++) {
    (void)fprintf(stderr, "[%i] %s\n", i, arguments[i]);
  }
  auto const location = find_caller_debug_location();
  (void)fprintf(
    stderr,
    "at %s:%i in %s: Hello, World!\n",
    location.file_path,
    location.line_number,
    location.function_name);
  return EXIT_SUCCESS;
}
