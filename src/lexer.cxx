#pragma once

#include "source.cxx"

#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <optional>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
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
    enum Variant
    {
      Uppercase,
      Lowercase,
    };

    std::string_view value;
    Variant variant;
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

    Location findLineStart() const
    {
      return Location{.index = index - column + 1, .line = line, .column = 1};
    }

    Location findLineEnd(std::string_view whole) const
    {
      auto iLineEnd = index + 1;
      while (iLineEnd < whole.size() && whole[iLineEnd] != '\n') { iLineEnd++; }
      return Location{
        .index = iLineEnd - 1,
        .line = line,
        .column = column + static_cast<std::uint32_t>(iLineEnd - 1 - index)};
    }
  };

  /// Linearly stored group of characters from a Thrice source.
  struct Portion
  {
    Location first;
    Location last;

    static Portion findLine(std::string_view whole, Location location)
    {
      return Portion{
        .first = location.findLineStart(), .last = location.findLineEnd(whole)};
    }

    std::string_view findValue(std::string_view whole) const
    {
      return whole.substr(first.index, findLength());
    }

    std::size_t findLength() const { return last.index - first.index + 1; }

    std::size_t findHeight() const { return last.line - first.line + 1; }
  };

  /// Positional information from a source file of the compiler. Not Thrice!
  struct CompilerLocation
  {
    std::string_view file;
    std::uint32_t line;
    std::string_view function;

    static CompilerLocation findCaller(
      std::string_view file = __builtin_FILE(),
      std::uint32_t line = __builtin_LINE(),
      std::string_view function = __builtin_FUNCTION())
    {
      file.remove_prefix(fullFilePath.length() - relativeFilePath.length());
      return CompilerLocation{.file = file, .line = line, .function = function};
    }

  private:
    static constexpr auto fullFilePath = std::string_view{__builtin_FILE()};
    static constexpr auto relativeFilePath = std::string_view{"lexer.cxx"};
  };

  /// Exceptional situation that stops the compilation.
  struct ThriceException: public std::exception
  {
    std::string explanation;

    template<typename... TFormattables>
    [[noreturn]] static void throwWithLocation(
      Source const& source,
      Location location,
      std::string_view title,
      std::tuple<TFormattables...> formattables,
      CompilerLocation caller = CompilerLocation::findCaller())
    {
      auto stream = std::ostringstream{};
      formatAll(
        stream,
        source.fullPath.string(),
        ':',
        location.line,
        ':',
        location.column,
        ": ",
        title,
        ": ",
        formattables,
        '\n');
      reportPortion(
        stream, source.contents, Portion{.first = location, .last = location});
      formatAll(
        stream,
        "\n[DEBUG] Emitted at ",
        caller.file,
        ':',
        caller.line,
        " in ",
        caller.function,
        '.');
      throw ThriceException{stream.str()};
    }

    template<typename... TFormattables>
    [[noreturn]] static void throwWithPortion(
      Source const& source,
      Portion portion,
      std::string_view title,
      std::tuple<TFormattables...> formattables,
      CompilerLocation caller = CompilerLocation::findCaller())
    {
      if (portion.findLength() == 1)
      {
        throwWithLocation(source, portion.first, title, formattables, caller);
      }

      auto stream = std::ostringstream{};
      formatAll(
        stream,
        source.fullPath.string(),
        ':',
        portion.first.line,
        ':',
        portion.first.column,
        ':',
        portion.last.line,
        ':',
        portion.last.column,
        ": ",
        title,
        ": ",
        formattables,
        '\n');
      reportPortion(stream, source.contents, portion);
      formatAll(
        stream,
        "\n[DEBUG] Emitted at ",
        caller.file,
        ':',
        caller.line,
        " in ",
        caller.function,
        '.');
      throw ThriceException{stream.str()};
    }

    char const* what() const noexcept override { return explanation.c_str(); }

  private:
    static constexpr auto lineNumberWidth = 10;

    static void
    reportPortion(auto& stream, std::string_view whole, Portion portion)
    {
      if (portion.findHeight() == 1)
      {
        underlinePortion(stream, whole, portion);
      }
      else
      {
        underlinePortion(
          stream,
          whole,
          Portion{
            .first = portion.first, .last = portion.first.findLineEnd(whole)});
        underlinePortion(
          stream,
          whole,
          Portion{.first = portion.last.findLineStart(), .last = portion.last},
          ContinuationStyle::Dotted);
      }
      stream << std::endl;
    }

    enum struct ContinuationStyle
    {
      None,
      Dotted,
    };

    static void underlinePortion(
      auto& stream,
      std::string_view whole,
      Portion portion,
      ContinuationStyle continuationStyle = ContinuationStyle::None)
    {
      auto line = Portion::findLine(whole, portion.first);

      formatAll(
        stream,
        std::setw(lineNumberWidth),
        continuationStyle == ContinuationStyle::Dotted ? "..." : " ",
        " |\n",
        std::setw(lineNumberWidth),
        portion.first.line,
        " | ",
        line.findValue(whole),
        '\n',
        std::setw(lineNumberWidth),
        ' ',
        " | ",
        std::setw(portion.first.column),
        '~');
      for (auto i = std::size_t{1}; i < portion.findLength(); i++)
      {
        stream << '~';
      }
    }

    static void formatAll(auto& stream) { (void)stream; }

    template<typename TFirstFormattable, typename... TRemainingFormattables>
    static void formatAll(
      auto& stream,
      TFirstFormattable firstFormattable,
      TRemainingFormattables... remainingFormattables)
    {
      formatOnce(stream, firstFormattable);
      formatAll(stream, remainingFormattables...);
    }

    template<typename TFormattable>
    static void formatOnce(auto& stream, TFormattable formattable)
    {
      // For not getting a warning when streaming string literals.
      if constexpr (std::is_same_v<std::decay_t<TFormattable>, char*>)
      {
        stream << (char const*)formattable;
      }
      else { stream << formattable; }
    }

    template<typename... TFormattables>
    static void formatOnce(auto& stream, std::tuple<TFormattables...> tuple)
    {
      formatTuple(stream, tuple, std::index_sequence_for<TFormattables...>());
    }

    template<typename... TFormattables, std::size_t... TIndices>
    static void formatTuple(
      auto& stream,
      std::tuple<TFormattables...> tuple,
      std::index_sequence<TIndices...> indices)
    {
      ((stream << std::get<TIndices>(tuple)), ...);
      (void)indices;
    }

    explicit ThriceException(std::string explanation):
      explanation{std::move(explanation)}
    {
    }
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

  /// Representation of a Thrice source file that went through the lexical
  /// analysis stage of the compiler.
  struct LexicalSource
  {
    Source source;
    std::vector<Lexeme> lexemes;
  };

  /// Context of the lexical analysis stage of the compiler.
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
    static constexpr auto decimalBase = std::int32_t{10};

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
      return isUppercase(character) || isLowercase(character);
    }

    static bool isUppercase(char character)
    {
      return character >= 'A' && character <= 'Z';
    }

    static bool isLowercase(char character)
    {
      return character >= 'a' && character <= 'z';
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
        case ';': lexMark(Mark::Semicolon); break;
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

          ThriceException::throwWithLocation(
            source,
            lStart,
            "error",
            std::tuple{"Unknown character '", cStart, "' in source!"});
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
        if (nFractionLength > INT32_MAX) { error("Huge number!"); }
        number.exponent = static_cast<std::int32_t>(-nFractionLength);
      }

      auto exponent = std::int32_t{0};

      // Lex the exponent part.
      if (take('e') || take('E'))
      {
        auto negative = take('-');
        if (!negative) { take('+'); }

        if (!isDigit(cCurrent)) { error("Incomplete number!"); }

        while (take(isDigit))
        {
          if (exponent > INT32_MAX / decimalBase) { error("Huge number!"); }
          exponent *= decimalBase;

          auto digit = static_cast<std::int32_t>(convertToDigit(cPrevious));
          if (exponent > INT32_MAX - digit) { error("Huge number!"); }
          exponent += digit;
        }

        if (negative) { exponent *= -1; }
      }

      if (exponent < INT32_MIN - number.exponent) { error("Huge number!"); }
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
          if (!take(isDigit)) { error("Incomplete number!"); }
        }
        else
        {
          if (!take(isDigit)) { break; }
        }

        if (mantissa > UINT64_MAX / decimalBase) { error("Huge number!"); }
        mantissa *= decimalBase;

        auto digit = convertToDigit(cPrevious);
        if (mantissa > UINT64_MAX - digit) { error("Huge number!"); }
        mantissa += digit;
      }
    }

    void lexWord()
    {
      while (take(isWordPart)) {}

      auto portion = findPreviousPortion();
      auto word = portion.findValue(source.contents);
      auto keyword = convertToKeyword(word);
      auto identifier =
        isUppercase(cStart) ? Identifier::Uppercase : Identifier::Lowercase;
      auto variant =
        keyword
          ? *keyword
          : Lexeme::Variant{Identifier{.value = word, .variant = identifier}};

      lexemes.push_back(Lexeme{.variant = variant, .portion = portion});
    }

    [[noreturn]] void error(
      auto formattable,
      CompilerLocation caller = CompilerLocation::findCaller()) const
    {
      error(std::tuple{formattable}, caller);
    }

    template<typename... TFormattables>
    [[noreturn]] void error(
      std::tuple<TFormattables...> formattables,
      CompilerLocation caller = CompilerLocation::findCaller()) const
    {
      ThriceException::throwWithPortion(
        source, findPreviousPortion(), "error", formattables, caller);
    }

    Portion findPreviousPortion() const
    {
      return Portion{.first = lStart, .last = lPrevious};
    }
  };
} // namespace rf
