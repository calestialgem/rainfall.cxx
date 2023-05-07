#pragma once

#include <cstddef>
#include <cstdint>
#include <cstdlib>

namespace rf {
  using Size = std::ptrdiff_t;

  template<typename TElement>
  struct ListView {
    TElement const* array;
    Size length;
  };

  template<typename TElement>
  struct List {
    TElement* array;
    Size capacity;
    Size length;
  };

  using Codepoint = std::uint32_t;
  using CodepointUnit = std::uint8_t;
  using StringView = ListView<CodepointUnit>;
  using String = List<CodepointUnit>;

  struct Arguments {
    String mainDirectory;
    String mainFile;
  };

  struct Source {
    String fullPath;
    String directory;
    String name;
    String contents;
  };

  using Line = std::uint32_t;
  using Column = std::uint32_t;

  struct Location {
    Size index;
    Line line;
    Column column;
  };

  struct Portion {
    Location first;
    Location last;
  };

  struct CompilerLocation {
    StringView file;
    StringView function;
    Line line;
    Column column;
  };

  using Mantissa = std::uint64_t;
  using Exponent = std::int32_t;

  struct Token {
    union {
      struct {
        Portion portion;
      };

      struct Private {
        Portion portion;
      } asPrivate;

      struct Public {
        Portion portion;
      } asPublic;

      struct Const {
        Portion portion;
      } asConst;

      struct Mut {
        Portion portion;
      } asMut;

      struct Auto {
        Portion portion;
      } asAuto;

      struct If {
        Portion portion;
      } asIf;

      struct Else {
        Portion portion;
      } asElse;

      struct Switch {
        Portion portion;
      } asSwitch;

      struct For {
        Portion portion;
      } asFor;

      struct While {
        Portion portion;
      } asWhile;

      struct Break {
        Portion portion;
      } asBreak;

      struct Continue {
        Portion portion;
      } asContinue;

      struct Return {
        Portion portion;
      } asReturn;

      struct OpeningParenthesis {
        Portion portion;
      } asOpeningParenthesis;

      struct ClosingParenthesis {
        Portion portion;
      } asClosingParenthesis;

      struct OpeningBrace {
        Portion portion;
      } asOpeningBrace;

      struct ClosingBrace {
        Portion portion;
      } asClosingBrace;

      struct OpeningBracket {
        Portion portion;
      } asOpeningBracket;

      struct ClosingBracket {
        Portion portion;
      } asClosingBracket;

      struct Colon {
        Portion portion;
      } asColon;

      struct ColonColon {
        Portion portion;
      } asColonColon;

      struct Semicolon {
        Portion portion;
      } asSemicolon;

      struct Comma {
        Portion portion;
      } asComma;

      struct Dot {
        Portion portion;
      } asDot;

      struct Star {
        Portion portion;
      } asStar;

      struct Slash {
        Portion portion;
      } asSlash;

      struct Percent {
        Portion portion;
      } asPercent;

      struct Plus {
        Portion portion;
      } asPlus;

      struct Minus {
        Portion portion;
      } asMinus;

      struct Ampersand {
        Portion portion;
      } asAmpersand;

      struct Caret {
        Portion portion;
      } asCaret;

      struct Pipe {
        Portion portion;
      } asPipe;

      struct Exclamation {
        Portion portion;
      } asExclamation;

      struct Left {
        Portion portion;
      } asLeft;

      struct Right {
        Portion portion;
      } asRight;

      struct Equal {
        Portion portion;
      } asEqual;

      struct StarEqual {
        Portion portion;
      } asStarEqual;

      struct SlashEqual {
        Portion portion;
      } asSlashEqual;

      struct PercentEqual {
        Portion portion;
      } asPercentEqual;

      struct PlusEqual {
        Portion portion;
      } asPlusEqual;

      struct MinusEqual {
        Portion portion;
      } asMinusEqual;

      struct AmpersandEqual {
        Portion portion;
      } asAmpersandEqual;

      struct CaretEqual {
        Portion portion;
      } asCaretEqual;

      struct PipeEqual {
        Portion portion;
      } asPipeEqual;

      struct ExclamationEqual {
        Portion portion;
      } asExclamationEqual;

      struct LeftEqual {
        Portion portion;
      } asLeftEqual;

      struct RightEqual {
        Portion portion;
      } asRightEqual;

      struct EqualEqual {
        Portion portion;
      } asEqualEqual;

      struct PlusPlus {
        Portion portion;
      } asPlusPlus;

      struct MinusMinus {
        Portion portion;
      } asMinusMinus;

      struct AmpersandAmpersand {
        Portion portion;
      } asAmpersandAmpersand;

      struct PipePipe {
        Portion portion;
      } asPipePipe;

      struct LeftLeft {
        Portion portion;
      } asLeftLeft;

      struct RightRight {
        Portion portion;
      } asRightRight;

      struct LeftLeftEqual {
        Portion portion;
      } asLeftLeftEqual;

      struct RightRightEqual {
        Portion portion;
      } asRightRightEqual;

      struct PascalCaseIdentifier {
        Portion portion;
        StringView value;
      } asPascalCaseIdentifier;

      struct CamelCaseIdentifier {
        Portion portion;
        StringView value;
      } asCamelCaseIdentifier;

      struct Number {
        Portion portion;
        Mantissa mantissa;
        Exponent exponent;
      } asNumber;
    };

    enum struct Tag {
      Private,
      Public,
      Const,
      Mut,
      Auto,
      If,
      Else,
      Switch,
      For,
      While,
      Break,
      Continue,
      Return,
      OpeningParenthesis,
      ClosingParenthesis,
      OpeningBrace,
      ClosingBrace,
      OpeningBracket,
      ClosingBracket,
      Colon,
      ColonColon,
      Semicolon,
      Comma,
      Dot,
      Star,
      Slash,
      Percent,
      Plus,
      Minus,
      Ampersand,
      Caret,
      Pipe,
      Exclamation,
      Left,
      Right,
      Equal,
      StarEqual,
      SlashEqual,
      PercentEqual,
      PlusEqual,
      MinusEqual,
      AmpersandEqual,
      CaretEqual,
      PipeEqual,
      ExclamationEqual,
      LeftEqual,
      RightEqual,
      EqualEqual,
      PlusPlus,
      MinusMinus,
      AmpersandAmpersand,
      PipePipe,
      LeftLeft,
      RightRight,
      LeftLeftEqual,
      RightRightEqual,
      PascalCaseIdentifier,
      CamelCaseIdentifier,
      Number,
    } tag;
  };
}
