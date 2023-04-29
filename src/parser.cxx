#pragma once

#include "lexer.cxx"

#include <memory>
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

    struct PrefixOperation
    {
      enum Variant
      {
        Promotion,
        Negation,
        BitwiseNot,
        LogicalNot,
      };

      std::unique_ptr<Expression> operand;
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

      std::unique_ptr<Expression> rightOperand;
      std::unique_ptr<Expression> leftOperand;
      Variant variant;
    };

    using Variant = std::variant<Literal, PrefixOperation, InfixOperation>;

    Variant variant;
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
      Type type;
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
    LexicalSource lexical;
    std::vector<Definition> definitions;

    static SyntacticalSource parse(LexicalSource lexical)
    {
      auto parser = Parser{.lexical = std::move(lexical)};

      parser.compute();

      return SyntacticalSource{
        .lexical = std::move(parser.lexical),
        .definitions = std::move(parser.definitions)};
    }

  private:
    void compute() {}
  };
}
