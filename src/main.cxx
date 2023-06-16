#include <stdio.h>
#include <stdlib.h>

int main(int const argument_count, char const* const* const arguments) {
  for (int i = 0; i < argument_count; i++) {
    (void)fprintf(stderr, "[%i] %s\n", i, arguments[i]);
  }
  return EXIT_SUCCESS;
}
