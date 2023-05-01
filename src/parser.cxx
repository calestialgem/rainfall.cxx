#pragma once

#include "lexer.cxx"
#include "utility.cxx"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

namespace rf
{
  struct PromotionOperation;
  struct NegationOperation;
  struct BitwiseNotOperation;
  struct LogicalNotOperation;
  struct PrefixIncrementOperation;
  struct PrefixDecrementOperation;
  struct PostfixIncrementOperation;
  struct PostfixDecrementOperation;
  struct MultiplicationOperation;
  struct DivisionOperation;
  struct ReminderOperation;
  struct AdditionOperation;
  struct SubtractionOperation;
  struct BitwiseAndOperation;
  struct BitwiseOrOperation;
  struct BitwiseXorOperation;
  struct LeftShiftOperation;
  struct RightShiftOperation;
  struct EqualOperation;
  struct NotEqualOperation;
  struct LessOperation;
  struct GreaterOperation;
  struct LessOrEqualOperation;
  struct GreaterOrEqualOperation;
  struct LogicalAndOperation;
  struct LogicalOrOperation;

  /// Instructions to compute a value.
  using Expression = std::variant<
    Number,
    CamelCaseIdentifier,
    PromotionOperation,
    NegationOperation,
    BitwiseNotOperation,
    LogicalNotOperation,
    PrefixIncrementOperation,
    PrefixDecrementOperation,
    PostfixIncrementOperation,
    PostfixDecrementOperation,
    MultiplicationOperation,
    DivisionOperation,
    ReminderOperation,
    AdditionOperation,
    SubtractionOperation,
    BitwiseAndOperation,
    BitwiseOrOperation,
    BitwiseXorOperation,
    LeftShiftOperation,
    RightShiftOperation,
    EqualOperation,
    NotEqualOperation,
    LessOperation,
    GreaterOperation,
    LessOrEqualOperation,
    GreaterOrEqualOperation,
    LogicalAndOperation,
    LogicalOrOperation>;

  struct PromotionOperation
  {
    Box<Expression> operand;
  };

  struct NegationOperation
  {
    Box<Expression> operand;
  };

  struct BitwiseNotOperation
  {
    Box<Expression> operand;
  };

  struct LogicalNotOperation
  {
    Box<Expression> operand;
  };

  struct PrefixIncrementOperation
  {
    Box<Expression> operand;
  };

  struct PrefixDecrementOperation
  {
    Box<Expression> operand;
  };

  struct PostfixIncrementOperation
  {
    Box<Expression> operand;
  };

  struct PostfixDecrementOperation
  {
    Box<Expression> operand;
  };

  struct MultiplicationOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct DivisionOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct ReminderOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct AdditionOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct SubtractionOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct BitwiseAndOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct BitwiseOrOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct BitwiseXorOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct LeftShiftOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct RightShiftOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct EqualOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct NotEqualOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct LessOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct GreaterOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct LessOrEqualOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct GreaterOrEqualOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct LogicalAndOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
  };

  struct LogicalOrOperation
  {
    Box<Expression> leftOperand;
    Box<Expression> rightOperand;
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

    using Base = std::variant<Auto, PascalCaseIdentifier>;

    Qualifier qualifier;
    Base base;
  };

  struct Binding;

  /// Pattern, which can be matched to a value or bind a value to a variable.
  using Pattern = std::variant<Expression, Formula, Binding>;

  struct Binding
  {
    Formula type;
    CamelCaseIdentifier name;
  };

  /// Entities in a Thrice program that define the symbols in the program.
  struct Definition
  {
    Pattern pattern;
    Expression bound;
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
      std::optional<Lexeme> lexeme;
    };

    LexicalSource lexical;
    std::vector<Definition> definitions;
    ReadLexeme current;
    ReadLexeme previous;

    static SyntacticalSource parse(LexicalSource lexical)
    {
      auto first = ReadLexeme{
        .lexeme = lexical.lexemes.empty() ? std::nullopt
                                          : std::optional{lexical.lexemes[0]}};
      auto parser = Parser{.lexical = std::move(lexical), .current = first};

      parser.compute();

      return SyntacticalSource{
        .lexical = std::move(parser.lexical),
        .definitions = std::move(parser.definitions)};
    }

  private:
    void compute()
    {
      while (current.lexeme)
      {
        if (auto definition = parseDefinition(); definition)
        {
          definitions.emplace_back(std::move(*definition));
          continue;
        }

        ThriceException::throwWithPortion(
          lexical.source,
          findPortion(*current.lexeme),
          "error",
          std::tuple{"Expected a definition in the global scope!"});
      }
    }

    std::optional<Definition> parseDefinition() { return std::nullopt; }

    std::optional<Formula> parseFormula()
    {
      auto qualifier = parseQualifier();

      if (auto b = take<PascalCaseIdentifier>(); b)
      {
        return Formula{.qualifier = qualifier, .base = *b};
      }
      if (auto b = take<Auto>(); b)
      {
        return Formula{.qualifier = qualifier, .base = *b};
      }

      if (qualifier != Formula::Qualifier::Plain) { error("formula base"); }

      return std::nullopt;
    }

    Formula::Qualifier parseQualifier()
    {
      if (take<Const>()) { return Formula::Qualifier::Const; }
      if (take<Mut>()) { return Formula::Qualifier::Mut; }
      if (take<Ampersand>())
      {
        if (take<Mut>()) { return Formula::Qualifier::RefMut; }
        return Formula::Qualifier::Ref;
      }
      return Formula::Qualifier::Plain;
    }

    std::optional<Expression> parseExpression() { return std::nullopt; }

    template<typename TLexeme>
    std::optional<TLexeme> take()
    {
      if (!current.lexeme || !std::holds_alternative<TLexeme>(*current.lexeme))
      {
        return std::nullopt;
      }
      advance();
      return std::get<TLexeme>(*current.lexeme);
    }

    void advance()
    {
      previous = current;
      if (++current.index < lexical.lexemes.size())
      {
        current.lexeme = lexical.lexemes[current.index];
      }
    }

    [[noreturn]] void error(std::string_view expected)
    {
      if (!current.lexeme)
      {
        ThriceException::throwWithPortion(
          lexical.source,
          findPortion(*previous.lexeme),
          "error",
          std::tuple{
            "Expected a ",
            expected,
            " after ",
            *previous.lexeme,
            ", at the end of the source file!"});
      }
      ThriceException::throwWithPortion(
        lexical.source,
        findPortion(*current.lexeme),
        "error",
        std::tuple{
          "Expected a ", expected, " instead of", *current.lexeme, "!"});
    }
  };
}
