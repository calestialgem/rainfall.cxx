#include "source.cxx"

#include <exception>
#include <filesystem>
#include <iostream>
#include <span>
#include <stdexcept>
#include <string_view>

/// Parsed command-line arguments.
struct Arguments
{
  explicit Arguments(std::span<char const* const> commandLineArguments)
  {
    switch (commandLineArguments.size())
    {
    case 0:
    case 1:
      throw std::runtime_error(
        "Usage: rainfall [main-directory: .] <main-file>");
    case 2:
      mainDirectory = ".";
      mainFile = commandLineArguments[1];
      break;
    case 3:
      mainDirectory = commandLineArguments[1];
      mainFile = commandLineArguments[2];
      break;
    default:
      throw std::runtime_error(
        "There could be at most 2 command line arguments!");
    }

    mainDirectory = mainDirectory.lexically_normal();
  }

  rf::Source loadMainSource()
  {
    return rf::Source::load(mainDirectory, mainFile);
  }

private:
  std::filesystem::path mainDirectory;
  std::string mainFile;
};

/// Entry into the compiler.
int main(int const argc, char const* const* const argv)
{
  try
  {
    auto arguments = Arguments(std::span(argv, argc));
    auto mainSource = arguments.loadMainSource();
    std::cout << "Source: " << mainSource.name << " in " << mainSource.directory
              << std::endl
              << "Full Path: " << mainSource.fullPath << std::endl
              << "Contents: " << std::endl
              << mainSource.contents;
  }
  catch (std::exception const& exception)
  {
    std::cerr << exception.what() << std::endl;
  }
}
