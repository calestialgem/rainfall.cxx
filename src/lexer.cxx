#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

namespace rf
{
  /// Reserved identifier that has a specific meaning in Thrice.
  struct Keyword
  {
    enum Type
    {
      Const,
      Auto,
    };

    Type type;
  };

  /// Specific pattern of characters that makes up an expression, definition or
  /// statement in Thrice.
  struct Mark
  {
    enum Type
    {
      OpeningBrace,
      ClosingBrace,
      OpeningParenthesis,
      ClosingParenthesis,
      Semicolon,
      Star,
      Slash,
      Percent,
      Plus,
      Minus,
      Tilde,
      Caret,

      Equal,
      EqualEqual,
      Ampersand,
      AmpersandAmpersand,
      Pipe,
      PipePipe,

      Exclamation,
      ExclamationEqual,

      Left,
      LeftEqual,
      LeftLeft,
      Right,
      RightEqual,
      RightRight,
    };

    Type type;
  };

  /// Name of a symbol in a Thrice program.
  struct Identifier
  {
  };

  /// Constant number that is embedded in a Thrice program.
  struct Number
  {
    std::uint64_t mantissa;
    std::int32_t exponent;
  };

  /// Indivisible structural element of a Thrice source.
  struct Lexeme
  {
    std::variant<Keyword, Mark, Identifier, Number> variant;
    std::string_view portion;
  };
}
