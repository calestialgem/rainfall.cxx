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

    static Portion merge(Portion leftMost, Portion rightMost)
    {
      return Portion{.first = leftMost.first, .last = rightMost.last};
    }

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

  struct Const
  {
    Portion portion;
  };

  struct Auto
  {
    Portion portion;
  };

  struct Mut
  {
    Portion portion;
  };

  struct OpeningBrace
  {
    Portion portion;
  };

  struct ClosingBrace
  {
    Portion portion;
  };

  struct OpeningParenthesis
  {
    Portion portion;
  };

  struct ClosingParenthesis
  {
    Portion portion;
  };

  struct Semicolon
  {
    Portion portion;
  };

  struct Star
  {
    Portion portion;
  };

  struct Slash
  {
    Portion portion;
  };

  struct Percent
  {
    Portion portion;
  };

  struct Plus
  {
    Portion portion;
  };

  struct Minus
  {
    Portion portion;
  };

  struct Tilde
  {
    Portion portion;
  };

  struct Caret
  {
    Portion portion;
  };

  struct Equal
  {
    Portion portion;
  };

  struct EqualEqual
  {
    Portion portion;
  };

  struct Ampersand
  {
    Portion portion;
  };

  struct AmpersandAmpersand
  {
    Portion portion;
  };

  struct Pipe
  {
    Portion portion;
  };

  struct PipePipe
  {
    Portion portion;
  };

  struct Exclamation
  {
    Portion portion;
  };

  struct ExclamationEqual
  {
    Portion portion;
  };

  struct Left
  {
    Portion portion;
  };

  struct LeftEqual
  {
    Portion portion;
  };

  struct LeftLeft
  {
    Portion portion;
  };

  struct Right
  {
    Portion portion;
  };

  struct RightEqual
  {
    Portion portion;
  };

  struct RightRight
  {
    Portion portion;
  };

  struct PascalCaseIdentifier
  {
    Portion portion;
    std::string_view value;
  };

  struct CamelCaseIdentifier
  {
    Portion portion;
    std::string_view value;
  };

  struct Number
  {
    Portion portion;
    std::uint64_t mantissa;
    std::int32_t exponent;
  };

  /// Indivisible structural element of a Thrice source.
  using Lexeme = std::variant<
    Const,
    Auto,
    Mut,
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
    PascalCaseIdentifier,
    CamelCaseIdentifier,
    Number>;

  Portion findPortion(Lexeme lexeme)
  {
    return std::visit([&](auto l) { return l.portion; }, lexeme);
  }

  auto& operator<<(auto& out, Lexeme const& lexeme)
  {
    out << '`';
    std::visit(
      [&](auto const& l)
      {
        using TLexeme = std::decay_t<decltype(l)>;
        if constexpr (std::is_same_v<TLexeme, Const>) { out << "const"; }
        else if constexpr (std::is_same_v<TLexeme, Auto>) { out << "auto"; }
        else if constexpr (std::is_same_v<TLexeme, Mut>) { out << "mut"; }
        else if constexpr (std::is_same_v<TLexeme, OpeningBrace>)
        {
          out << "{";
        }
        else if constexpr (std::is_same_v<TLexeme, ClosingBrace>)
        {
          out << "}";
        }
        else if constexpr (std::is_same_v<TLexeme, OpeningParenthesis>)
        {
          out << "(";
        }
        else if constexpr (std::is_same_v<TLexeme, ClosingParenthesis>)
        {
          out << ")";
        }
        else if constexpr (std::is_same_v<TLexeme, Semicolon>) { out << ";"; }
        else if constexpr (std::is_same_v<TLexeme, Star>) { out << "*"; }
        else if constexpr (std::is_same_v<TLexeme, Slash>) { out << "/"; }
        else if constexpr (std::is_same_v<TLexeme, Percent>) { out << "%"; }
        else if constexpr (std::is_same_v<TLexeme, Plus>) { out << "+"; }
        else if constexpr (std::is_same_v<TLexeme, Minus>) { out << "-"; }
        else if constexpr (std::is_same_v<TLexeme, Tilde>) { out << "~"; }
        else if constexpr (std::is_same_v<TLexeme, Caret>) { out << "^"; }
        else if constexpr (std::is_same_v<TLexeme, Equal>) { out << "="; }
        else if constexpr (std::is_same_v<TLexeme, EqualEqual>) { out << "=="; }
        else if constexpr (std::is_same_v<TLexeme, Ampersand>) { out << "&"; }
        else if constexpr (std::is_same_v<TLexeme, AmpersandAmpersand>)
        {
          out << "&&";
        }
        else if constexpr (std::is_same_v<TLexeme, Pipe>) { out << "|"; }
        else if constexpr (std::is_same_v<TLexeme, PipePipe>) { out << "||"; }
        else if constexpr (std::is_same_v<TLexeme, Exclamation>) { out << "!"; }
        else if constexpr (std::is_same_v<TLexeme, ExclamationEqual>)
        {
          out << "!=";
        }
        else if constexpr (std::is_same_v<TLexeme, Left>) { out << "<"; }
        else if constexpr (std::is_same_v<TLexeme, LeftEqual>) { out << "<="; }
        else if constexpr (std::is_same_v<TLexeme, LeftLeft>) { out << "<<"; }
        else if constexpr (std::is_same_v<TLexeme, Right>) { out << ">"; }
        else if constexpr (std::is_same_v<TLexeme, RightEqual>) { out << ">="; }
        else if constexpr (std::is_same_v<TLexeme, RightRight>) { out << ">>"; }
        else if constexpr (
          std::is_same_v<TLexeme, PascalCaseIdentifier> ||
          std::is_same_v<TLexeme, CamelCaseIdentifier>)
        {
          out << l.value;
        }
        else if constexpr (std::is_same_v<TLexeme, Number>)
        {
          out << l.mantissa;
          if (l.exponent != 0) { out << 'e' << l.exponent; }
        }
      },
      lexeme);
    return out << '`';
  }

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
    struct ReadLocation
    {
      Location location;
      char character;
    };

    Source source;
    std::vector<Lexeme> lexemes;
    ReadLocation current;
    ReadLocation previous;
    ReadLocation initial;

    static LexicalSource lex(Source source)
    {
      auto initial = source.contents.empty() ? '\0' : source.contents[0];
      auto first = ReadLocation{
        .location = Location{.line = 1, .column = 1}, .character = initial};
      auto lexer = Lexer{.source = std::move(source), .current = first};

      lexer.compute();

      return LexicalSource{std::move(lexer.source), std::move(lexer.lexemes)};
    }

  private:
    static constexpr auto decimalBase = std::int32_t{10};

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
        // initial character of the next lexeme; thus, take it
        // unconditionally and remember the next lexeme's initial.
        advance();
        initial = previous;

        switch (initial.character)
        {
        case ' ':
        case '\t':
        case '\n': break;
        case '#':
          // Advance over the characters upto a newline; then, take it.
          while (!take('\n')) { advance(); }
          break;
        case '{': lexSingleMark<OpeningBrace>(); break;
        case '}': lexSingleMark<ClosingBrace>(); break;
        case '(': lexSingleMark<OpeningParenthesis>(); break;
        case ')': lexSingleMark<ClosingParenthesis>(); break;
        case ';': lexSingleMark<Semicolon>(); break;
        case '*': lexSingleMark<Star>(); break;
        case '/': lexSingleMark<Slash>(); break;
        case '%': lexSingleMark<Percent>(); break;
        case '+': lexSingleMark<Plus>(); break;
        case '-': lexSingleMark<Minus>(); break;
        case '~': lexSingleMark<Tilde>(); break;
        case '^': lexSingleMark<Caret>(); break;
        case '=': lexRepeatableMark<Equal, EqualEqual>(); break;
        case '&': lexRepeatableMark<Ampersand, AmpersandAmpersand>(); break;
        case '|': lexRepeatableMark<Pipe, PipePipe>(); break;
        case '!': lexExtensibleMark<Exclamation, ExclamationEqual>(); break;
        case '<':
          lexExtensibleOrRepeatableMark<Left, LeftEqual, LeftLeft>();
          break;
        case '>':
          lexExtensibleOrRepeatableMark<Right, RightEqual, RightRight>();
          break;
        default:
          if (isDigit(initial.character))
          {
            lexNumber();
            break;
          }

          if (isWordInitial(initial.character))
          {
            lexWord();
            break;
          }

          ThriceException::throwWithLocation(
            source,
            initial.location,
            "error",
            std::tuple{
              "Unknown character '", initial.character, "' in source!"});
        }
      }
    }

    void advance()
    {
      previous = current;

      current.character =
        hasCurrent() ? source.contents[++current.location.index] : '\0';

      // Update the line number after the new line character.
      if (previous.character == '\n')
      {
        current.location.line++;
        current.location.column = 1;
      }
      else { current.location.column++; }
    }

    bool take(auto predicate)
    {
      if (!predicate(current.character)) { return false; }
      advance();
      return true;
    }

    bool take(char character)
    {
      return take([&](char c) { return c == character; });
    }

    bool hasCurrent() const
    {
      return current.location.index < source.contents.length();
    }

    template<typename TSingle>
    void lexSingleMark()
    {
      lexemes.push_back(Lexeme{TSingle{.portion = findPreviousPortion()}});
    }

    template<typename TSingle, typename TExtended>
    void lexExtensibleMark()
    {
      lexemes.push_back(
        take('=') ? TExtended{.portion = findPreviousPortion()}
                  : Lexeme{TSingle{.portion = findPreviousPortion()}});
    }

    template<typename TSingle, typename TRepeated>
    void lexRepeatableMark()
    {
      lexemes.push_back(
        take(initial.character)
          ? TRepeated{.portion = findPreviousPortion()}
          : Lexeme{TSingle{.portion = findPreviousPortion()}});
    }

    template<typename TSingle, typename TExtended, typename TRepeated>
    void lexExtensibleOrRepeatableMark()
    {
      lexemes.push_back(
        take('=') ? TExtended{.portion = findPreviousPortion()}
        : take(initial.character)
          ? TRepeated{.portion = findPreviousPortion()}
          : Lexeme{TSingle{.portion = findPreviousPortion()}});
    }

    void lexNumber()
    {
      auto number = Number{.mantissa = convertToDigit(initial.character)};

      // Lex the whole part.
      lexMantissa(number.mantissa);

      // Lex the fractional part.
      // Cache current location to roll back the taken '.'. It can be a
      // member access instead of fraction separator.
      if (auto previousAtDot = previous, currentAtDot = current; take('.'))
      {
        if (!isDigit(current.character))
        {
          // Rollback the taken '.' and end the number.
          previous = previousAtDot;
          current = currentAtDot;
          number.portion = findPreviousPortion();
          lexemes.emplace_back(number);
        }

        auto iFractionBegin = current.location.index;
        lexMantissa(number.mantissa);
        auto iFractionEnd = current.location.index;

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

        if (!isDigit(current.character)) { error("Incomplete number!"); }

        while (take(isDigit))
        {
          if (exponent > INT32_MAX / decimalBase) { error("Huge number!"); }
          exponent *= decimalBase;

          auto digit =
            static_cast<std::int32_t>(convertToDigit(previous.character));
          if (exponent > INT32_MAX - digit) { error("Huge number!"); }
          exponent += digit;
        }

        if (negative) { exponent *= -1; }
      }

      if (exponent < INT32_MIN - number.exponent) { error("Huge number!"); }
      number.exponent += exponent;

      number.portion = findPreviousPortion();
      lexemes.emplace_back(number);
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

        auto digit = convertToDigit(previous.character);
        if (mantissa > UINT64_MAX - digit) { error("Huge number!"); }
        mantissa += digit;
      }
    }

    void lexWord()
    {
      while (take(isWordPart)) {}

      auto portion = findPreviousPortion();
      auto word = portion.findValue(source.contents);

      if (word == "const") { lexemes.emplace_back(Const{.portion = portion}); }
      else if (word == "auto")
      {
        lexemes.emplace_back(Auto{.portion = portion});
      }
      else if (word == "mut") { lexemes.emplace_back(Mut{.portion = portion}); }
      else if (isUppercase(word[0]))
      {
        lexemes.emplace_back(
          PascalCaseIdentifier{.portion = portion, .value = word});
      }
      else
      {
        lexemes.emplace_back(
          CamelCaseIdentifier{.portion = portion, .value = word});
      }
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
      return Portion{.first = initial.location, .last = previous.location};
    }
  };
}
