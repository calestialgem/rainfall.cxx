#pragma once

namespace rf
{
  /// Provides a test message for debugging project setup.
  [[nodiscard]] char const* provide_message() noexcept
  {
    return "Hello, World!";
  }
}
