#pragma once

#include <filesystem>
#include <fstream>
#include <ios>
#include <string>

namespace rf
{
  /// Contents of a source file as a linear, ordered collection of characters.
  struct Contents
  {
    std::string characters;
  };

  /// Representation of a Thrice source.
  class Source
  {
  public:
    Source(std::filesystem::path const& directory, std::string name):
      path(directory / (std::move(name) + ".tr")), contents()
    {
      auto inputStream = std::ifstream(path);
      inputStream >> contents.characters;
    }

  private:
    std::filesystem::path path;
    Contents contents;
  };
}
