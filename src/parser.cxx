#pragma once

#include "lexer.cxx"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

namespace rf
{
  /// Instructions to compute a value.
  struct Expression
  {
    struct Literal
    {
      Number number;
    };

    struct Accessing
    {
      Identifier accessed;
    };

    struct Grouping
    {
    };

    struct PrefixOperation
    {
      enum Variant
      {
        Promotion,
        Negation,
        BitwiseNot,
        LogicalNot,
        Increment,
        Decrement,
      };

      Variant variant;
    };

    struct PostfixOperation
    {
      enum Variant
      {
        Increment,
        Decrement,
      };

      Variant variant;
    };

    struct InfixOperation
    {
      enum Variant
      {
        Multiplication,
        Division,
        Reminder,
        Addition,
        Subtraction,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        LeftShift,
        RightShift,
        Equal,
        NotEqual,
        Less,
        Greater,
        LessOrEqual,
        GreaterOrEqual,
        LogicalAnd,
        LogicalOr,
      };

      Variant variant;
    };

    using Sub = std::variant<
      Literal,
      Accessing,
      Grouping,
      PrefixOperation,
      PostfixOperation,
      InfixOperation>;

    std::vector<Sub> subs;
  };

  /// Applications to construct a type.
  struct Formula
  {
    enum Qualifier
    {
      Const,
      Mut,
      Ref,
      RefMut,
      Plain,
    };

    struct Inferred
    {
    };

    struct Named
    {
      Identifier name;
    };

    using Base = std::variant<Inferred, Named>;

    Qualifier qualifier;
    Base base;
  };

  /// Pattern, which can be matched to a value or bind a value to a variable.
  struct Pattern
  {
    struct Type
    {
      Formula expected;
    };

    struct Binding
    {
      Formula type;
      Identifier name;
    };

    struct Matching
    {
      Expression matched;
    };

    using Variant = std::variant<Type, Binding, Matching>;

    Variant variant;
  };

  /// Entities in a Thrice program that define the symbols in the program.
  struct Definition
  {
    struct Binding
    {
      Pattern pattern;
      Expression bound;
    };

    using Variant = std::variant<Binding>;

    Variant variant;
  };

  /// Representation of a Thrice source file that when through the syntactical
  /// analysis stage of the compiler.
  struct SyntacticalSource
  {
    LexicalSource lexical;
    std::vector<Definition> definitions;
  };

  /// Context of the syntactical analysis stage of the compiler.
  struct Parser
  {
    struct ReadLexeme
    {
      std::size_t index;
      Lexeme lexeme;
    };

    LexicalSource lexical;
    std::vector<Definition> definitions;
    ReadLexeme current;
    ReadLexeme previous;

    static SyntacticalSource parse(LexicalSource lexical)
    {
      auto initial = lexical.lexemes.empty() ? Lexeme{} : lexical.lexemes[0];
      auto first = ReadLexeme{.index = 0, .lexeme = initial};
      auto parser = Parser{.lexical = std::move(lexical), .current = first};

      parser.compute();

      return SyntacticalSource{
        .lexical = std::move(parser.lexical),
        .definitions = std::move(parser.definitions)};
    }

  private:
    void compute()
    {
      while (hasCurrent())
      {
        if (auto definition = parseDefinition(); definition)
        {
          definitions.emplace_back(std::move(*definition));
        }

        ThriceException::throwWithPortion(
          lexical.source,
          current.lexeme.portion,
          "error",
          std::tuple{"Expected a definition in the global scope!"});
      }
    }

    std::optional<Definition> parseDefinition() { return std::nullopt; }

    bool hasCurrent() const { return current.index < lexical.lexemes.size(); }
  };
}
