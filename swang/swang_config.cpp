#include "swang_config.hpp"
#include <vector>

#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"

static char const DEBUG_TYPE[] = "swang";

template<>
struct llvm::yaml::ScalarEnumerationTraits<swang::language_type> {
  static void enumeration(IO & io, swang::language_type & value) {
    io.enumCase(value, "any", swang::language_type::none);
    io.enumCase(value, "c++", swang::language_type::cpp);
    io.enumCase(value, "C++", swang::language_type::cpp);
  }
};

template<>
struct llvm::yaml::ScalarEnumerationTraits<swang::casing_type> {
  static void enumeration(IO & io, swang::casing_type & value) {
    io.enumCase(value, "any", swang::casing_type::any_case);
    io.enumCase(value, "aNy_CasE", swang::casing_type::any_case);
    io.enumCase(value, "lower", swang::casing_type::lower_case);
    io.enumCase(value, "lower_case", swang::casing_type::lower_case);
    io.enumCase(value, "upper", swang::casing_type::upper_case);
    io.enumCase(value, "UPPER_CASE", swang::casing_type::upper_case);
    io.enumCase(value, "camel", swang::casing_type::camel_case);
    io.enumCase(value, "CamelCase", swang::casing_type::camel_case);
    io.enumCase(value, "camel_back", swang::casing_type::camel_back);
    io.enumCase(value, "camelBack", swang::casing_type::camel_back);
  }
};

template<>
struct llvm::yaml::MappingTraits<swang::config::style> {
  static void mapping(llvm::yaml::IO & io, swang::config::style & style) {
    auto predefined = std::string();

    io.mapOptional("casing", style.casing);
    io.mapOptional("prefix", style.prefix);
    io.mapOptional("suffix", style.suffix);

    io.setContext(static_cast<void*>(&style));
  }
};

template<>
struct llvm::yaml::MappingTraits<swang::config> {
  static void mapping(llvm::yaml::IO & io, swang::config & style) {
    io.mapRequired("language", style.language);
    io.mapOptional("namespace", style.namespace_style);
    io.mapOptional("record", style.record_style);
    io.mapOptional("record_template", style.record_template_style);
    io.mapOptional("specialization", style.specialization_style);
    io.mapOptional("declarator", style.declarator_style);
    io.mapOptional("parameter", style.parameter_style);
    io.mapOptional("access", style.access_style);
    io.mapOptional("constructor", style.constructor_style);
    io.mapOptional("destructor", style.destructor_style);
    io.mapOptional("enum", style.enum_style);
    io.mapOptional("constant", style.constant_style);
    io.mapOptional("method", style.method_style);
    io.mapOptional("variable", style.variable_style);
    io.mapOptional("field", style.field_style);
    io.mapOptional("function", style.function_style);
    io.mapOptional("function_template", style.function_template_style);
    io.mapOptional("friend", style.friend_style);
    io.mapOptional("using", style.using_style);
    io.mapOptional("unresolved", style.unresolved_style);
    io.mapOptional("named", style.named_style);
    io.mapOptional("decl", style.decl_style);
    io.setContext(static_cast<void*>(&style));
  }
};

template<>
struct llvm::yaml::DocumentListTraits < std::vector < swang::config >> {
  static size_t size(IO& io, std::vector<swang::config>& sequence) {
    return sequence.size();
  }

  static swang::config& element(IO&, std::vector<swang::config>& sequence, size_t index) {
    if (index >= sequence.size())
      sequence.resize(index + 1);
    return sequence[index];
  }

};

namespace {

  static llvm::StringRef get_language_name(swang::language_type language) {
    switch (language) {
      case swang::language_type::cpp:
        return "C++";

      default:
        return "Unknown";
    }
  }

  static swang::language_type get_language_by_file_name(llvm::StringRef file_name) {
    return swang::language_type::cpp;
  }

  static bool get_predefined_config(llvm::StringRef name, swang::language_type language, swang::config* style) {
    // if (Name.equals_lower("llvm"))
    // *style = getLLVMStyle();
    // else if (Name.equals_lower("chromium"))
    // *style = getChromiumStyle(language);
    // else if (Name.equals_lower("mozilla"))
    // *style = getMozillaStyle();
    // else if (Name.equals_lower("google"))
    // *style = getGoogleStyle(language);
    // else if (Name.equals_lower("webkit"))
    // *style = getWebKitStyle();
    // else if (Name.equals_lower("gnu"))
    // *style = getGNUStyle();
    // else if (Name.equals_lower("none"))
    // *style = getNoStyle();
    // else
    // return false;

    *style = swang::config();
    style->language = language;
    return true;
  }

  static std::error_code parse_config(llvm::StringRef text, swang::config* style) {
    assert(style);

    auto const language = style->language;
    assert(language != swang::language_type::none);

    if (text.trim().empty())
      return std::make_error_code(std::errc::invalid_argument);

    auto              styles = std::vector<swang::config>();
    llvm::yaml::Input input(text);

    input.setContext(style);
    input >> styles;

    if (input.error())
      return input.error();

    for (auto const& s : styles) {
      if ((s.language == swang::language_type::none) && (&s != &styles.back()))
        return std::make_error_code(std::errc::invalid_argument);

      for (auto const& t : styles) {
        if ((s.language == t.language) && (&s != &t)) {
//          DEBUG(llvm::dbgs() << "Duplicate languages in the config file on positions " << (&s - styles.begin()) << " and " << (&t - styles.begin()) << "\n");
          return std::make_error_code(std::errc::invalid_argument);
        }
      }
    }

    for (auto const& s : styles) {
      if ((s.language == language) || (s.language == swang::language_type::none)) {
        *style          = s;
        style->language = language;
        return std::error_code();
      }
    }

    return std::make_error_code(std::errc::not_supported);
  }

}

namespace swang {

  swang::config get_config(std::string name, std::string file_name, std::string fallback_name) {
    auto style = swang::config();

    style.language = get_language_by_file_name(file_name);
    if (!get_predefined_config(fallback_name, style.language, &style)) {
      llvm::errs() << "Invalid fallback style \"" << fallback_name << "\" using LLVM style\n";
      return style;
    }

    auto config_name = llvm::StringRef(name);
    if (config_name.startswith("{")) {
      // Parse YAML/JSON style from the command line.
      if (std::error_code ec = parse_config(config_name, &style))
        llvm::errs() << "Error parsing -style: " << ec.message() << ", using " << fallback_name << " style\n";
      return style;
    }

    if (!config_name.equals_lower("file")) {
      if (!get_predefined_config(config_name, style.language, &style))
        llvm::errs() << "Invalid value for -style, using " << fallback_name << " style\n";

      return style;
    }

    // Look for .swang file in the file's parent directories.
    auto unsuitable_config_files = llvm::SmallString<128>();
    auto path = llvm::SmallString<128>(file_name);

    llvm::sys::fs::make_absolute(path);
    for (llvm::StringRef directory = path; !directory.empty(); directory = llvm::sys::path::parent_path(directory)) {
      if (!llvm::sys::fs::is_directory(directory))
        continue;

      auto config_file = llvm::SmallString<128>(directory);
      llvm::sys::path::append(config_file, ".swang");
      DEBUG(llvm::dbgs() << "Trying " << config_file << "...\n");
      bool is_file = false;

      // Ignore errors from is_regular_file: we only need to know if we can read the file or not.
      llvm::sys::fs::is_regular_file(llvm::Twine(config_file), is_file);

      if (is_file) {
        auto buffer = llvm::MemoryBuffer::getFile(config_file.c_str(), config_file.size());
        if (buffer.getError()) {
          llvm::errs() << buffer.getError().message() << "\n";
          break;
        }

        if (auto ec = parse_config(buffer.get()->getBuffer(), &style)) {
          if (ec == std::errc::not_supported) {
            if (!unsuitable_config_files.empty())
              unsuitable_config_files.append(", ");
            unsuitable_config_files.append(config_file);
            continue;
          }

          llvm::errs() << "Error reading " << config_file << ": " << ec.message() << "\n";
          break;
        }

        DEBUG(llvm::dbgs() << "Using configuration file " << config_file << "\n");
        return style;
      }
    }

    llvm::errs() << "Can't find usable .swang, using " << fallback_name << " style\n";
    if (!unsuitable_config_files.empty())
      llvm::errs() << "Configuration file(s) do(es) not support " << get_language_name(style.language) << ": " << unsuitable_config_files << "\n";

    return style;
  }

}
