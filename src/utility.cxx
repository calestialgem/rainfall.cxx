#pragma once

#include <utility>

namespace rf
{
  /// Dynamicly allocated container that always holds a single element.
  template<typename TElement>
  struct Box
  {
    TElement* pointer;

    [[nodiscard]] constexpr Box(): pointer{new TElement{}} {}

    [[nodiscard]] constexpr Box(TElement value):
      pointer{new TElement{std::move(value)}}
    {
    }

    [[nodiscard]] constexpr Box(Box const& other):
      pointer{new TElement{*other.pointer}}
    {
    }

    [[nodiscard]] constexpr Box(Box&& other) noexcept:
      pointer{std::exchange(other.pointer, nullptr)}
    {
    }

    constexpr ~Box() noexcept
    {
      if (pointer) { return; }
      pointer->~TElement();
      delete pointer;
    }

    constexpr Box& operator=(Box other)
    {
      std::swap(pointer, other.pointer);
      return *this;
    }

    constexpr Box& operator=(Box&& other) noexcept
    {
      std::swap(pointer, other.pointer);
      return *this;
    }

    [[nodiscard]] constexpr operator TElement&() noexcept { return *pointer; }

    [[nodiscard]] constexpr operator TElement const&() const noexcept
    {
      return *pointer;
    }

    [[nodiscard]] constexpr TElement& operator*() noexcept { return *pointer; }

    [[nodiscard]] constexpr TElement const& operator*() const noexcept
    {
      return *pointer;
    }

    [[nodiscard]] constexpr TElement* operator->() noexcept { return pointer; }

    [[nodiscard]] constexpr TElement const* operator->() const noexcept
    {
      return pointer;
    }
  };
}
