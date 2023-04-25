#pragma once

#include "source.cxx"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace rf
{
  /// Reserved identifier that has a specific meaning in Thrice.
  enum struct Keyword
  {
    Const,
    Auto,
  };

  /// Specific pattern of characters that makes up an expression, definition or
  /// statement in Thrice.
  enum struct Mark
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

  /// Name of a symbol in a Thrice program.
  struct Identifier
  {
    std::string_view value;
  };

  /// Constant number that is embedded in a Thrice program.
  struct Number
  {
    std::uint64_t mantissa;
    std::int32_t exponent;
  };

  /// Positional information of a character in a Thrice source.
  struct Location
  {
    std::size_t index;
    std::uint32_t line;
    std::uint32_t column;
  };

  /// Linearly stored group of characters from a Thrice source.
  struct Portion
  {
    Location first;
    Location last;

    std::string_view findValue(std::string_view whole) const
    {
      return whole.substr(first.index, findLength());
    }

    std::size_t findLength() const { return last.index - first.index + 1; }
  };

  /// Indivisible structural element of a Thrice source.
  struct Lexeme
  {
    using Variant = std::variant<Keyword, Mark, Identifier, Number>;

    template<class... Lambdas>
    struct Visitor: Lambdas...
    {
      using Lambdas::operator()...;
    };
    template<class... Ts>
    Visitor(Ts...) -> Visitor<Ts...>;

    Variant variant;
    Portion portion;
  };

  struct LexicalSource
  {
    Source source;
    std::vector<Lexeme> lexemes;
  };

  struct Lexer
  {
    Source source;
    std::vector<Lexeme> lexemes;
    Location lCurrent;
    Location lPrevious;
    Location lStart;
    char cCurrent;
    char cPrevious;
    char cStart;

    static LexicalSource lex(Source source)
    {
      auto cCurrent = source.contents.empty() ? '\0' : source.contents[0];
      auto lexer = Lexer{
        .source = std::move(source),
        .lCurrent = Location{.line = 1, .column = 1},
        .cCurrent = cCurrent};

      lexer.compute();

      return LexicalSource{std::move(lexer.source), std::move(lexer.lexemes)};
    }

  private:
    static constexpr auto DECIMAL_BASE = std::int32_t{10};

    static void addToMark(Mark& mark, int amount = 1)
    {
      mark = static_cast<Mark>(static_cast<int>(mark) + amount);
    }

    static std::optional<Keyword> convertToKeyword(std::string_view word)
    {
      if (word == "const") { return Keyword::Const; }
      if (word == "auto") { return Keyword::Auto; }
      return std::nullopt;
    }

    static bool isWordPart(char character)
    {
      return isWordInitial(character) || isDigit(character);
    }

    static bool isWordInitial(char character)
    {
      return (character >= 'a' && character <= 'z') ||
             (character >= 'A' && character <= 'Z') || character == '_';
    }

    static bool isDigit(char character)
    {
      return character >= '0' && character <= '9';
    }

    static std::uint64_t convertToDigit(char character)
    {
      return character - '0';
    }

    void compute()
    {
      while (hasCurrent())
      {
        // Since all lexemes are at least 1 character long, the current
        // character is for sure included in the next lexeme and it is the
        // initial character of the next lexeme; thus, take it unconditionally
        // and remember the next lexeme's initial.
        advance();
        lStart = lPrevious;
        cStart = cPrevious;

        switch (cStart)
        {
        case ' ':
        case '\t':
        case '\n': break;
        case '#':
          // Advance over the characters upto a newline; then, take it.
          while (!take('\n')) { advance(); }
          break;
        case '{': lexMark(Mark::OpeningBrace); break;
        case '}': lexMark(Mark::ClosingBrace); break;
        case '(': lexMark(Mark::OpeningParenthesis); break;
        case ')': lexMark(Mark::ClosingParenthesis); break;
        case ':': lexMark(Mark::Semicolon); break;
        case '*': lexMark(Mark::Star); break;
        case '/': lexMark(Mark::Slash); break;
        case '%': lexMark(Mark::Percent); break;
        case '+': lexMark(Mark::Plus); break;
        case '-': lexMark(Mark::Minus); break;
        case '~': lexMark(Mark::Tilde); break;
        case '^': lexMark(Mark::Caret); break;
        case '=': lexMark(Mark::Equal, MarkVariant::DOUBLE); break;
        case '&': lexMark(Mark::Ampersand, MarkVariant::DOUBLE); break;
        case '|': lexMark(Mark::Pipe, MarkVariant::DOUBLE); break;
        case '!': lexMark(Mark::Exclamation, MarkVariant::EQUAL); break;
        case '<': lexMark(Mark::Left, MarkVariant::EQUAL_OR_DOUBLE); break;
        case '>': lexMark(Mark::Right, MarkVariant::EQUAL_OR_DOUBLE); break;
        default:
          if (isDigit(cStart))
          {
            lexNumber();
            break;
          }

          if (isWordInitial(cStart))
          {
            lexWord();
            break;
          }

          throw std::invalid_argument{"Unknown character!"};
        }
      }
    }

    void advance()
    {
      lPrevious = lCurrent;
      cPrevious = cCurrent;

      cCurrent = hasCurrent() ? source.contents[++lCurrent.index] : '\0';

      // Update the line number after the new line character.
      if (cPrevious == '\n')
      {
        lCurrent.line++;
        lCurrent.column = 1;
      }
      else { lCurrent.column++; }
    }

    bool take(auto predicate)
    {
      if (!predicate(cCurrent)) { return false; }
      advance();
      return true;
    }

    bool take(char character)
    {
      return take([&](char c) { return c == character; });
    }

    bool hasCurrent() const
    {
      return lCurrent.index < source.contents.length();
    }

    enum struct MarkVariant
    {
      SINGLE,
      DOUBLE,
      EQUAL,
      EQUAL_OR_DOUBLE,
    };

    void lexMark(Mark mark, MarkVariant variant = MarkVariant::SINGLE)
    {
      switch (variant)
      {
      case MarkVariant::SINGLE: break;
      case MarkVariant::EQUAL:
        if (take('=')) { addToMark(mark); }
        break;
      case MarkVariant::DOUBLE:
        if (take(cStart)) { addToMark(mark); }
        break;
      case MarkVariant::EQUAL_OR_DOUBLE:
        if (take(cStart)) { addToMark(mark, 2); }
        else if (take('=')) { addToMark(mark); }
        break;
      }

      lexemes.push_back(
        Lexeme{.variant = mark, .portion = findPreviousPortion()});
    }

    void lexNumber()
    {
      auto number = Number{.mantissa = convertToDigit(cStart)};

      // Lex the whole part.
      lexMantissa(number.mantissa);

      // Cache current location to roll back the taken '.'. It can be a member
      // access instead of fraction separator.
      auto lPreviousAtDot = lPrevious;
      auto cPreviousAtDot = cPrevious;
      auto lCurrentAtDot = lCurrent;
      auto cCurrentAtDot = cCurrent;

      // Lex the fractional part.
      if (take('.'))
      {
        if (!isDigit(cCurrent))
        {
          // Rollback the taken '.' and end the number.
          lPrevious = lPreviousAtDot;
          cPrevious = cPreviousAtDot;
          lCurrent = lCurrentAtDot;
          cCurrent = cCurrentAtDot;
          lexemes.push_back(
            Lexeme{.variant = number, .portion = findPreviousPortion()});
        }

        auto iFractionBegin = lCurrent.index;
        lexMantissa(number.mantissa);
        auto iFractionEnd = lCurrent.index;

        auto nFractionLength = iFractionEnd - iFractionBegin;
        if (nFractionLength > INT32_MAX)
        {
          throw std::invalid_argument{"Huge number!"};
        }
        number.exponent = static_cast<std::int32_t>(-nFractionLength);
      }

      auto exponent = std::int32_t{0};

      // Lex the exponent part.
      if (take('e') || take('E'))
      {
        auto negative = take('-');
        if (!negative) { take('+'); }

        if (!isDigit(cCurrent))
        {
          throw std::invalid_argument{"Incomplete number!"};
        }

        while (take(isDigit))
        {
          if (exponent > INT32_MAX / DECIMAL_BASE)
          {
            throw std::invalid_argument{"Huge number!"};
          }
          exponent *= DECIMAL_BASE;

          auto digit = static_cast<std::int32_t>(convertToDigit(cPrevious));
          if (exponent > INT32_MAX - digit)
          {
            throw std::invalid_argument{"Huge number!"};
          }
          exponent += digit;
        }

        if (negative) { exponent *= -1; }
      }

      if (number.exponent < INT32_MIN - exponent)
      {
        throw std::invalid_argument{"Huge number!"};
      }
      number.exponent += exponent;

      lexemes.push_back(
        Lexeme{.variant = number, .portion = findPreviousPortion()});
    }

    void lexMantissa(std::uint64_t& mantissa)
    {
      while (true)
      {
        if (take('_'))
        {
          if (!take(isDigit))
          {
            throw std::invalid_argument{"Incomplete number!"};
          }
        }
        else
        {
          if (!take(isDigit)) { break; }
        }

        if (mantissa > UINT64_MAX / DECIMAL_BASE)
        {
          throw std::invalid_argument{"Huge number!"};
        }
        mantissa *= DECIMAL_BASE;

        auto digit = convertToDigit(cPrevious);
        if (mantissa > UINT64_MAX - digit)
        {
          throw std::invalid_argument{"Huge number!"};
        }
        mantissa += digit;
      }
    }

    void lexWord()
    {
      while (take(isWordPart)) {}

      auto portion = findPreviousPortion();
      auto word = portion.findValue(source.contents);
      auto keyword = convertToKeyword(word);
      auto variant =
        keyword ? *keyword : Lexeme::Variant{Identifier{.value = word}};

      lexemes.push_back(Lexeme{.variant = variant, .portion = portion});
    }

    Portion findPreviousPortion() const
    {
      return Portion{.first = lStart, .last = lPrevious};
    }
  };
} // namespace rf
