#pragma once

#include "lexer.cxx"
#include "utility.cxx"

#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace rf
{
  struct Grouping;
  struct PromotionOperation;
  struct NegationOperation;
  struct BitwiseNotOperation;
  struct LogicalNotOperation;
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
    Grouping,
    PromotionOperation,
    NegationOperation,
    BitwiseNotOperation,
    LogicalNotOperation,
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

  struct Grouping
  {
    OpeningParenthesis opening;
    Box<Expression> grouped;
    ClosingParenthesis closing;
  };

  struct PromotionOperation
  {
    Plus op;
    Box<Expression> operand;
  };

  struct NegationOperation
  {
    Minus op;
    Box<Expression> operand;
  };

  struct BitwiseNotOperation
  {
    Tilde op;
    Box<Expression> operand;
  };

  struct LogicalNotOperation
  {
    Exclamation op;
    Box<Expression> operand;
  };

  struct MultiplicationOperation
  {
    Box<Expression> leftOperand;
    Star op;
    Box<Expression> rightOperand;
  };

  struct DivisionOperation
  {
    Box<Expression> leftOperand;
    Slash op;
    Box<Expression> rightOperand;
  };

  struct ReminderOperation
  {
    Box<Expression> leftOperand;
    Percent op;
    Box<Expression> rightOperand;
  };

  struct AdditionOperation
  {
    Box<Expression> leftOperand;
    Plus op;
    Box<Expression> rightOperand;
  };

  struct SubtractionOperation
  {
    Box<Expression> leftOperand;
    Minus op;
    Box<Expression> rightOperand;
  };

  struct BitwiseAndOperation
  {
    Box<Expression> leftOperand;
    Ampersand op;
    Box<Expression> rightOperand;
  };

  struct BitwiseOrOperation
  {
    Box<Expression> leftOperand;
    Pipe op;
    Box<Expression> rightOperand;
  };

  struct BitwiseXorOperation
  {
    Box<Expression> leftOperand;
    Caret op;
    Box<Expression> rightOperand;
  };

  struct LeftShiftOperation
  {
    Box<Expression> leftOperand;
    LeftLeft op;
    Box<Expression> rightOperand;
  };

  struct RightShiftOperation
  {
    Box<Expression> leftOperand;
    RightRight op;
    Box<Expression> rightOperand;
  };

  struct EqualOperation
  {
    Box<Expression> leftOperand;
    EqualEqual op;
    Box<Expression> rightOperand;
  };

  struct NotEqualOperation
  {
    Box<Expression> leftOperand;
    ExclamationEqual op;
    Box<Expression> rightOperand;
  };

  struct LessOperation
  {
    Box<Expression> leftOperand;
    Left op;
    Box<Expression> rightOperand;
  };

  struct GreaterOperation
  {
    Box<Expression> leftOperand;
    Right op;
    Box<Expression> rightOperand;
  };

  struct LessOrEqualOperation
  {
    Box<Expression> leftOperand;
    LeftEqual op;
    Box<Expression> rightOperand;
  };

  struct GreaterOrEqualOperation
  {
    Box<Expression> leftOperand;
    RightEqual op;
    Box<Expression> rightOperand;
  };

  struct LogicalAndOperation
  {
    Box<Expression> leftOperand;
    AmpersandAmpersand op;
    Box<Expression> rightOperand;
  };

  struct LogicalOrOperation
  {
    Box<Expression> leftOperand;
    PipePipe op;
    Box<Expression> rightOperand;
  };

  Portion findPortion(Expression const& expression)
  {
    return std::visit(
      [](auto const& e)
      {
        using TExpression = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<TExpression, Grouping>)
        {
          return Portion::merge(e.opening.portion, e.closing.portion);
        }
        else if constexpr (
          std::is_same_v<TExpression, PromotionOperation> ||
          std::is_same_v<TExpression, NegationOperation> ||
          std::is_same_v<TExpression, BitwiseNotOperation> ||
          std::is_same_v<TExpression, LogicalNotOperation>)
        {
          return Portion::merge(e.op.portion, findPortion(*e.operand));
        }
        else if constexpr (
          std::is_same_v<TExpression, MultiplicationOperation> ||
          std::is_same_v<TExpression, DivisionOperation> ||
          std::is_same_v<TExpression, ReminderOperation> ||
          std::is_same_v<TExpression, AdditionOperation> ||
          std::is_same_v<TExpression, SubtractionOperation> ||
          std::is_same_v<TExpression, BitwiseAndOperation> ||
          std::is_same_v<TExpression, BitwiseOrOperation> ||
          std::is_same_v<TExpression, BitwiseXorOperation> ||
          std::is_same_v<TExpression, LeftShiftOperation> ||
          std::is_same_v<TExpression, RightShiftOperation> ||
          std::is_same_v<TExpression, EqualOperation> ||
          std::is_same_v<TExpression, NotEqualOperation> ||
          std::is_same_v<TExpression, LessOperation> ||
          std::is_same_v<TExpression, GreaterOperation> ||
          std::is_same_v<TExpression, LessOrEqualOperation> ||
          std::is_same_v<TExpression, GreaterOrEqualOperation> ||
          std::is_same_v<TExpression, LogicalAndOperation> ||
          std::is_same_v<TExpression, LogicalOrOperation>)
        {
          return Portion::merge(
            findPortion(*e.leftOperand), findPortion(*e.rightOperand));
        }
        else { return e.portion; }
      },
      expression);
  }

  struct RefMut;

  using Qualifier = std::variant<Const, Mut, Ampersand, RefMut>;

  struct RefMut
  {
    Ampersand ref;
    Mut mut;
  };

  Portion findPortion(Qualifier const& qualifier)
  {
    return std::visit(
      [](auto const& q)
      {
        using TQualifier = std::decay_t<decltype(q)>;
        if constexpr (std::is_same_v<TQualifier, RefMut>)
        {
          return Portion::merge(q.ref.portion, q.mut.portion);
        }
        else { return q.portion; }
      },
      qualifier);
  }

  using Base = std::variant<Auto, PascalCaseIdentifier>;

  Portion findPortion(Base const& base)
  {
    return std::visit([](auto const& b) { return b.portion; }, base);
  }

  /// Applications to construct a type.
  struct Formula
  {
    std::optional<Qualifier> qualifier;
    Base base;
  };

  Portion findPortion(Formula const& formula)
  {
    auto basePortion = findPortion(formula.base);
    if (!formula.qualifier) { return basePortion; }

    auto qualifierPortion = findPortion(*formula.qualifier);
    return Portion::merge(qualifierPortion, basePortion);
  }

  struct Binding;

  /// Pattern, which can be matched to a value or bind a value to a variable.
  using Pattern = std::variant<Expression, Formula, Binding>;

  struct Binding
  {
    Formula type;
    CamelCaseIdentifier name;
  };

  Portion findPortion(Pattern const& pattern)
  {
    return std::visit(
      [](auto const& p)
      {
        using TPattern = std::decay_t<decltype(p)>;
        if constexpr (std::is_same_v<TPattern, Binding>)
        {
          return Portion::merge(findPortion(p.type), p.name.portion);
        }
        else { return findPortion(p); }
      },
      pattern);
  }

  /// Entities in a Thrice program that define the symbols in the program.
  struct Definition
  {
    Pattern pattern;
    Equal separator;
    Expression bound;
    Semicolon terminator;
  };

  Portion findPortion(Definition const& definition)
  {
    return Portion::merge(
      findPortion(definition.pattern), definition.terminator.portion);
  }

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

    std::optional<Definition> parseDefinition()
    {
      if (auto pattern = parsePattern(); pattern)
      {
        if (!std::holds_alternative<Binding>(*pattern))
        {
          reportWrong(
            findPortion(*pattern),
            "Definition must have a binding in its pattern!");
        }

        auto separator = take<Equal>();
        if (!separator)
        {
          reportIncomplete(findPortion(*pattern), "definition", "`=`");
        }

        auto bound = parseExpression();
        if (!bound)
        {
          reportIncomplete(
            Portion::merge(findPortion(*pattern), separator->portion),
            "definition",
            "expression");
        }

        auto terminator = take<Semicolon>();
        if (!terminator)
        {
          reportIncomplete(
            Portion::merge(findPortion(*pattern), findPortion(*bound)),
            "definition",
            "`;`");
        }

        return Definition{
          .pattern = std::move(*pattern),
          .separator = *separator,
          .bound = std::move(*bound),
          .terminator = *terminator};
      }

      return std::nullopt;
    }

    std::optional<Pattern> parsePattern()
    {
      if (auto formula = parseFormula(); formula)
      {
        if (auto name = take<CamelCaseIdentifier>(); name)
        {
          return Binding{.type = *formula, .name = *name};
        }

        return *formula;
      }

      if (auto expression = parseExpression(); expression)
      {
        return *expression;
      }

      return std::nullopt;
    }

    std::optional<Formula> parseFormula()
    {
      auto qualifier = parseQualifier();

      if (auto base = take<PascalCaseIdentifier>(); base)
      {
        return Formula{.qualifier = qualifier, .base = *base};
      }

      if (auto base = take<Auto>(); base)
      {
        return Formula{.qualifier = qualifier, .base = *base};
      }

      if (qualifier)
      {
        reportIncomplete(findPortion(*qualifier), "formula", "base");
      }

      return std::nullopt;
    }

    std::optional<Qualifier> parseQualifier()
    {
      if (auto qualifier = take<Const>(); qualifier) { return *qualifier; }
      if (auto qualifier = take<Mut>(); qualifier) { return *qualifier; }
      if (auto qualifier = take<Ampersand>(); qualifier)
      {
        if (auto extension = take<Mut>(); extension)
        {
          return RefMut{.ref = *qualifier, .mut = *extension};
        }
        return *qualifier;
      }
      return std::nullopt;
    }

    std::optional<Expression> parseExpression() { return parseLogicalOr(); }

    std::optional<Expression> parseLogicalOr()
    {
      return parseInfixOperation<
        &Parser::parseLogicalAnd,
        LogicalOrOperation>();
    }

    std::optional<Expression> parseLogicalAnd()
    {
      return parseInfixOperation<
        &Parser::parseBitwiseOr,
        LogicalAndOperation>();
    }

    std::optional<Expression> parseBitwiseOr()
    {
      return parseInfixOperation<
        &Parser::parseBitwiseXor,
        BitwiseOrOperation>();
    }

    std::optional<Expression> parseBitwiseXor()
    {
      return parseInfixOperation<
        &Parser::parseBitwiseAnd,
        BitwiseXorOperation>();
    }

    std::optional<Expression> parseBitwiseAnd()
    {
      return parseInfixOperation<
        &Parser::parseEqualityComparison,
        BitwiseAndOperation>();
    }

    std::optional<Expression> parseEqualityComparison()
    {
      return parseInfixOperation<
        &Parser::parseOrderComparison,
        EqualOperation,
        NotEqualOperation>();
    }

    std::optional<Expression> parseOrderComparison()
    {
      return parseInfixOperation<
        &Parser::parseShift,
        LessOperation,
        GreaterOperation,
        LessOrEqualOperation,
        GreaterOrEqualOperation>();
    }

    std::optional<Expression> parseShift()
    {
      return parseInfixOperation<
        &Parser::parseAdditive,
        LeftShiftOperation,
        RightShiftOperation>();
    }

    std::optional<Expression> parseAdditive()
    {
      return parseInfixOperation<
        &Parser::parseMultiplicative,
        AdditionOperation,
        SubtractionOperation>();
    }

    std::optional<Expression> parseMultiplicative()
    {
      return parseInfixOperation<
        &Parser::parseUnary,
        MultiplicationOperation,
        DivisionOperation,
        ReminderOperation>();
    }

    enum struct InfixOperationParseResult
    {
      NotFound,
      Parsed,
    };

    template<auto operandParser, typename... TOperations>
    std::optional<Expression> parseInfixOperation()
    {
      auto leftOperand = (this->*operandParser)();
      if (!leftOperand) { return std::nullopt; }

      auto result = InfixOperationParseResult::NotFound;

      ((parseSingleInfixOperation<
         &Parser::parseInfixOperation<operandParser, TOperations...>,
         TOperations>(result, leftOperand)),
       ...);

      return leftOperand;
    }

    template<auto operandParser, typename TOperation>
    void parseSingleInfixOperation(
      InfixOperationParseResult& result, std::optional<Expression>& leftOperand)
    {
      if (result == InfixOperationParseResult::Parsed) { return; }

      auto op = take<std::decay_t<decltype(TOperation::op)>>();
      if (!op) { return; }

      auto rightOperand = (this->*operandParser)();
      if (!rightOperand)
      {
        reportIncomplete(
          Portion::merge(findPortion(*leftOperand), op->portion),
          "infix operation",
          "expression");
      }

      leftOperand = std::move(TOperation{
        .leftOperand = std::move(*leftOperand),
        .op = *op,
        .rightOperand = std::move(*rightOperand)});

      result = InfixOperationParseResult::Parsed;
    }

    std::optional<Expression> parseUnary()
    {
      if (auto op = take<Plus>(); op)
      {
        auto operand = parseUnary();
        if (!operand)
        {
          reportIncomplete(op->portion, "promotion", "expression");
        }

        return PromotionOperation{.op = *op, .operand = std::move(*operand)};
      }

      if (auto op = take<Minus>(); op)
      {
        auto operand = parseUnary();
        if (!operand)
        {
          reportIncomplete(op->portion, "negation", "expression");
        }

        return NegationOperation{.op = *op, .operand = std::move(*operand)};
      }

      if (auto op = take<Tilde>(); op)
      {
        auto operand = parseUnary();
        if (!operand)
        {
          reportIncomplete(op->portion, "bitwise not", "expression");
        }

        return BitwiseNotOperation{.op = *op, .operand = std::move(*operand)};
      }

      if (auto op = take<Exclamation>(); op)
      {
        auto operand = parseUnary();
        if (!operand)
        {
          reportIncomplete(op->portion, "logical not", "expression");
        }

        return LogicalNotOperation{.op = *op, .operand = std::move(*operand)};
      }

      return parsePrimary();
    }

    std::optional<Expression> parsePrimary()
    {
      if (auto number = take<Number>(); number) { return *number; }

      if (auto identifier = take<CamelCaseIdentifier>(); identifier)
      {
        return *identifier;
      }

      if (auto opening = take<OpeningParenthesis>(); opening)
      {
        auto grouped = parseExpression();
        if (!grouped)
        {
          reportIncomplete(opening->portion, "grouping", "expression");
        }

        auto closing = take<ClosingParenthesis>();
        if (!closing)
        {
          reportIncomplete(
            Portion::merge(opening->portion, findPortion(*grouped)),
            "grouping",
            "`)`");
        }

        return Grouping{
          .opening = *opening,
          .grouped = std::move(*grouped),
          .closing = *closing};
      }

      return std::nullopt;
    }

    template<typename TLexeme>
    std::optional<TLexeme> take()
    {
      if (!current.lexeme || !std::holds_alternative<TLexeme>(*current.lexeme))
      {
        return std::nullopt;
      }
      auto taken = std::get<TLexeme>(*current.lexeme);
      advance();
      return taken;
    }

    void advance()
    {
      previous = current;
      if (++current.index < lexical.lexemes.size())
      {
        current.lexeme = lexical.lexemes[current.index];
      }
      else { current.lexeme = std::nullopt; }
    }

    [[noreturn]] void reportWrong(
      Portion wrongPortion,
      std::string_view explanation,
      CompilerLocation caller = CompilerLocation::findCaller()) const
    {
      ThriceException::throwWithPortion(
        lexical.source, wrongPortion, "error", std::tuple{explanation}, caller);
    }

    [[noreturn]] void reportIncomplete(
      Portion incompletePortion,
      std::string_view incompleteName,
      std::string_view continuationName,
      CompilerLocation caller = CompilerLocation::findCaller()) const
    {
      if (!current.lexeme)
      {
        ThriceException::throwWithPortion(
          lexical.source,
          incompletePortion,
          "error",
          std::tuple{
            "Expected ",
            continuationName,
            " to complete ",
            incompleteName,
            ", at the end of the file!"},
          caller);
      }
      ThriceException::throwWithPortion(
        lexical.source,
        incompletePortion,
        "error",
        std::tuple{
          "Expected ",
          continuationName,
          " to complete ",
          incompleteName,
          ", instead of ",
          *current.lexeme,
          "!"},
        caller);
    }
  };
}
