#include "lexer.cxx"
#include "parser.cxx"
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
    auto mainDefinitions = rf::Parser::parse(mainLexemes);

    for (auto const& definition: mainDefinitions.definitions)
    {
      std::cout << definition << std::endl;
    }
  }
  catch (std::exception const& exception)
  {
    std::cerr << exception.what() << std::endl;
  }
}
