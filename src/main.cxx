#include "lexer.cxx"
#include "source.cxx"

#include <exception>
#include <filesystem>
#include <iostream>
#include <span>
#include <stdexcept>
#include <variant>

/// Parsed command-line arguments.
struct Arguments
{
  std::filesystem::path mainDirectory;
  std::string mainFile;

  static Arguments parse(std::span<char const* const> commandLineArguments)
  {
    switch (commandLineArguments.size())
    {
    case 0:
    case 1:
      throw std::invalid_argument(
        "Usage: rainfall [main-directory: .] <main-file>");
    case 2: return Arguments{".", commandLineArguments[1]};
    case 3:
      return Arguments{
        std::filesystem::path(commandLineArguments[1]).lexically_normal(),
        commandLineArguments[2]};
    default:
      throw std::invalid_argument(
        "There could be at most 2 command line arguments!");
    }
  }

  rf::Source loadMainSource() const
  {
    return rf::Source::load(mainDirectory, mainFile);
  }
};

/// Entry into the compiler.
int main(int argc, char const* const* argv)
{
  try
  {
    auto arguments = Arguments::parse(std::span(argv, argc));
    auto mainSource = arguments.loadMainSource();
    auto mainLexemes = rf::Lexer::lex(mainSource);

    std::cout << "Lexemes of " << mainLexemes.source.name << std::endl;
    for (auto const& lexeme: mainLexemes.lexemes)
    {
      std::visit(
        rf::Lexeme::Visitor{
          [](rf::Keyword const& l)
          {
            switch (l)
            {
            case rf::Keyword::Const: std::cout << "const"; break;
            case rf::Keyword::Auto: std::cout << "auto"; break;
            }
          },
          [](rf::Mark const& l)
          {
            switch (l)
            {
            case rf::Mark::OpeningBrace: std::cout << "{"; break;
            case rf::Mark::ClosingBrace: std::cout << "}"; break;
            case rf::Mark::OpeningParenthesis: std::cout << "("; break;
            case rf::Mark::ClosingParenthesis: std::cout << ")"; break;
            case rf::Mark::Semicolon: std::cout << ";"; break;
            case rf::Mark::Star: std::cout << "*"; break;
            case rf::Mark::Slash: std::cout << "/"; break;
            case rf::Mark::Percent: std::cout << "%"; break;
            case rf::Mark::Plus: std::cout << "+"; break;
            case rf::Mark::Minus: std::cout << "-"; break;
            case rf::Mark::Tilde: std::cout << "~"; break;
            case rf::Mark::Caret: std::cout << "^"; break;
            case rf::Mark::Equal: std::cout << "="; break;
            case rf::Mark::EqualEqual: std::cout << "=="; break;
            case rf::Mark::Ampersand: std::cout << "&"; break;
            case rf::Mark::AmpersandAmpersand: std::cout << "&&"; break;
            case rf::Mark::Pipe: std::cout << "|"; break;
            case rf::Mark::PipePipe: std::cout << "||"; break;
            case rf::Mark::Exclamation: std::cout << "!"; break;
            case rf::Mark::ExclamationEqual: std::cout << "!="; break;
            case rf::Mark::Left: std::cout << "<"; break;
            case rf::Mark::LeftEqual: std::cout << "<="; break;
            case rf::Mark::LeftLeft: std::cout << "<<"; break;
            case rf::Mark::Right: std::cout << ">"; break;
            case rf::Mark::RightEqual: std::cout << ">="; break;
            case rf::Mark::RightRight: std::cout << ">>"; break;
            }
          },
          [](rf::Identifier const& l) { std::cout << '\"' << l.value << '\"'; },
          [](rf::Number const& l)
          {
            std::cout << l.mantissa;
            if (l.exponent != 0) { std::cout << "x10^" << l.exponent; }
          }},
        lexeme.variant);
      std::cout << '\n';
    }
  }
  catch (std::exception const& exception)
  {
    std::cerr << exception.what() << std::endl;
  }
}
