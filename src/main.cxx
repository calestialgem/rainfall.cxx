#include <cstdio>

int main(int const argument_count, char const* const* const arguments) {
  for (int i = 0; i < argument_count; i++) {
    (void)std::fprintf(stderr, "[%i] %s\n", i, arguments[i]);
  }
  return 0;
}
