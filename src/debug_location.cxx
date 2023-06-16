#include "utility.hxx"

static Byte const* convert_to_relative_path(Byte const* absolute_path);

DebugLocation find_caller_debug_location(
  Byte const* const absolute_path,
  Integer32 const line_number,
  Byte const* const function_name) {
  Byte const* const relative_path = convert_to_relative_path(absolute_path);
  auto const caller_location =
    DebugLocation(relative_path, line_number, function_name);
  return caller_location;
}

Byte const* convert_to_relative_path(Byte const* const absolute_path) {
  Byte const* const current_absolute_path = __builtin_FILE();
  Byte const* const current_relative_path = "debug_location.cxx";

  auto const current_absolute_bytes = strlen(current_absolute_path);
  auto const current_relative_bytes = strlen(current_relative_path);
  auto const common_bytes = current_absolute_bytes - current_relative_bytes;

  Byte const* const relative_path = absolute_path + common_bytes;
  return relative_path;
}
