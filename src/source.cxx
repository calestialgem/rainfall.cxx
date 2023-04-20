#pragma once

#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <string>

namespace rf
{
  /// Representation of a Thrice source.
  struct Source
  {
    Source(std::filesystem::path const& directory, std::string name):
      path(directory / (std::move(name) + ".tr"))
    {
      auto inputStream = std::ifstream(path);
      while (!inputStream.eof())
      {
        std::string line;
        std::getline(inputStream, line);
        contents += line;
        contents += '\n';
      }
    }

    void debugPrint()
    {
      std::cout << "Source: " << path << " Contents:" << std::endl
                << contents << std::endl;
    }

  private:
    std::filesystem::path path;
    std::string contents;
  };
}
