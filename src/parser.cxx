#pragma once

#include "lexer.cxx"

#include <utility>
#include <vector>

namespace rf
{
  /// Entities in a Thrice program that define the symbols in the program.
  struct Definition
  {
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
