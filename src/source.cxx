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
    std::filesystem::path fullPath;
    std::filesystem::path directory;
    std::string name;
    std::string contents;

    static Source load(std::filesystem::path directory, std::string name)
    {
      // Construct the full path by adding the file extension.
      auto fullPath = directory / (name + ".tr");

      // Read all the lines in the file.
      auto contents = std::string();
      auto inputStream = std::ifstream(fullPath);
      while (!inputStream.eof())
      {
        std::string line;
        std::getline(inputStream, line);
        contents += line;
        contents += '\n';
      }

      return Source{
        std::move(fullPath),
        std::move(directory),
        std::move(name),
        std::move(contents)};
    }
  };
}
