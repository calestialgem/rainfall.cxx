#pragma once

#include <stdint.h>
#include <string.h>

using Byte = char;
using Integer32 = int32_t;
using Integer64 = int64_t;
using Size = Integer64;

struct DebugLocation {
  Byte const* file_path;
  Integer32 line_number;
  Byte const* function_name;
};

DebugLocation find_caller_debug_location(
  Byte const* absolute_path = __builtin_FILE(),
  Integer32 line_number = __builtin_LINE(),
  Byte const* function_name = __builtin_FUNCTION());
