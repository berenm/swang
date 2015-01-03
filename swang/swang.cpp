#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/YAMLTraits.h"

#include <cctype>

namespace swang {
  namespace casing {

    enum type {
      any_case=0,
      lower_case,
      camel_back,
      upper_case,
      camel_case,
    };

    static char const* const names[] = {
      "aNy_CasE",
      "lower_case",
      "camelBack",
      "UPPER_CASE",
      "CamelCase",
    };

    static llvm::Regex matchers[] = {
      llvm::Regex(llvm::StringRef("^.*$")),
      llvm::Regex(llvm::StringRef("^[a-z][a-z0-9_]*$")),
      llvm::Regex(llvm::StringRef("^[a-z][a-zA-Z0-9]*$")),
      llvm::Regex(llvm::StringRef("^[A-Z][A-Z0-9_]*$")),
      llvm::Regex(llvm::StringRef("^[A-Z][a-zA-Z0-9]*$")),
    };

    static auto splitter = llvm::Regex(llvm::StringRef("(([a-z0-9A-Z]*)(_+)|([A-Z]?[a-z0-9]+)([A-Z]|$)|([A-Z]+)([A-Z]|$))"));

  }

  struct config {
    struct style {
      style() :
        is_set(false),
        casing(casing::type::any_case),
        prefix(""),
        suffix("") {}

      style(bool is_set, casing::type casing, std::string prefix, std::string suffix) :
        is_set(is_set),
        casing(casing),
        prefix(std::move(prefix)),
        suffix(std::move(suffix)) {}

      bool         is_set;
      casing::type casing;
      std::string  prefix;
      std::string  suffix;
    };

    style namespace_style;
    style inline_namespace_style;

    style enum_constant_style;
    style constexpr_variable_style;
    style member_constant_style;
    style member_style;
    style class_constant_style;
    style class_member_style;
    style global_constant_style;
    style global_variable_style;
    style local_constant_style;
    style local_variable_style;
    style static_constant_style;
    style static_variable_style;
    style constant_style;
    style variable_style;

    style parameter_constant_style;
    style parameter_pack_style;
    style parameter_style;

    style abstract_style;
    style pod_style;
    style struct_style;
    style class_style;
    style union_style;
    style enum_style;

    style pure_function_style;
    style global_function_style;
    style constexpr_function_style;
    style function_style;

    style pure_method_style;
    style constexpr_method_style;
    style virtual_method_style;
    style class_method_style;
    style method_style;

    style typedef_style;
    style template_using_style;
    style using_style;

    style type_template_parameter_style;
    style value_template_parameter_style;
    style template_template_parameter_style;
    style template_parameter_style;
  };

} // namespace swang

namespace llvm {
  namespace yaml {

    template< >
    struct ScalarEnumerationTraits< swang::casing::type > {
      static void enumeration(IO& io, swang::casing::type& value) {
        io.enumCase(value, "any", swang::casing::type::any_case);
        io.enumCase(value, "aNy_CasE", swang::casing::type::any_case);
        io.enumCase(value, "lower", swang::casing::type::lower_case);
        io.enumCase(value, "lower_case", swang::casing::type::lower_case);
        io.enumCase(value, "upper", swang::casing::type::upper_case);
        io.enumCase(value, "UPPER_CASE", swang::casing::type::upper_case);
        io.enumCase(value, "camel", swang::casing::type::camel_case);
        io.enumCase(value, "CamelCase", swang::casing::type::camel_case);
        io.enumCase(value, "camel_back", swang::casing::type::camel_back);
        io.enumCase(value, "camelBack", swang::casing::type::camel_back);
      }

    };

    template< >
    struct MappingTraits< swang::config::style > {
      static void mapping(IO& io, swang::config::style& style) {
        auto predefined = std::string();

        io.mapOptional("casing", style.casing);
        io.mapOptional("prefix", style.prefix);
        io.mapOptional("suffix", style.suffix);
        style.is_set = true;

        io.setContext(static_cast< void* >(&style));
      }

    };

    template< >
    struct MappingTraits< swang::config > {
      static void mapping(IO& io, swang::config& style) {
        io.mapOptional("namespace", style.namespace_style);
        io.mapOptional("inline_namespace", style.inline_namespace_style);

        io.mapOptional("enum_constant", style.enum_constant_style);
        io.mapOptional("constexpr", style.constexpr_variable_style);
        io.mapOptional("member_constant", style.member_constant_style);
        io.mapOptional("member", style.member_style);
        io.mapOptional("class_constant", style.class_constant_style);
        io.mapOptional("class_member", style.class_member_style);
        io.mapOptional("global_constant", style.global_constant_style);
        io.mapOptional("global_variable", style.global_variable_style);
        io.mapOptional("local_constant", style.local_constant_style);
        io.mapOptional("local_variable", style.local_variable_style);
        io.mapOptional("static_constant", style.static_constant_style);
        io.mapOptional("static_variable", style.static_variable_style);
        io.mapOptional("constant", style.constant_style);
        io.mapOptional("variable", style.variable_style);

        io.mapOptional("parameter_constant", style.parameter_constant_style);
        io.mapOptional("parameter_pack", style.parameter_pack_style);
        io.mapOptional("parameter", style.parameter_style);

        io.mapOptional("abstract", style.abstract_style);
        io.mapOptional("pod", style.pod_style);
        io.mapOptional("struct", style.struct_style);
        io.mapOptional("class", style.class_style);
        io.mapOptional("union", style.union_style);
        io.mapOptional("enum", style.enum_style);

        io.mapOptional("pure_function", style.pure_function_style);
        io.mapOptional("global_function", style.global_function_style);
        io.mapOptional("constexpr_function", style.constexpr_function_style);
        io.mapOptional("function", style.function_style);

        io.mapOptional("pure_method", style.pure_method_style);
        io.mapOptional("constexpr_method", style.constexpr_method_style);
        io.mapOptional("virtual_method", style.virtual_method_style);
        io.mapOptional("class_method", style.class_method_style);
        io.mapOptional("method", style.method_style);

        io.mapOptional("typedef", style.typedef_style);
        io.mapOptional("template_using", style.template_using_style);
        io.mapOptional("using", style.using_style);

        io.mapOptional("type_template_parameter", style.type_template_parameter_style);
        io.mapOptional("value_template_parameter", style.value_template_parameter_style);
        io.mapOptional("template_template_parameter", style.template_template_parameter_style);
        io.mapOptional("template_parameter", style.template_parameter_style);

        io.setContext(static_cast< void* >(&style));
      } // mapping

    };

    template< >
    struct DocumentListTraits< std::vector< swang::config > > {
      static size_t size(IO& io, std::vector< swang::config >& sequence) {
        return sequence.size();
      }

      static swang::config& element(IO&, std::vector< swang::config >& sequence, size_t index) {
        if (index >= sequence.size())
          sequence.resize(index + 1);
        return sequence[index];
      }

    };

  } // namespace yaml
} // namespace llvm

namespace swang {
  namespace {

    auto const DEBUG_TYPE = "swang";

    namespace cfg {

      auto parse = [](llvm::StringRef text, std::error_code& error) {
                     if (text.trim().empty())
                       return error = std::make_error_code(std::errc::invalid_argument), swang::config();

                     auto styles = std::vector< swang::config >();

                     llvm::yaml::Input input(text);
                     input >> styles;

                     if (input.error())
                       return error = input.error(), swang::config();

                     if (styles.size() == 0)
                       return error = std::make_error_code(std::errc::not_supported), swang::config();

                     return error = std::error_code(), std::move(styles[0]);
                   };

      auto find = [](std::string filename) {
                    // Look for .swang file in the file's parent directories.
                    auto path = llvm::SmallString< 128 >(filename);

                    llvm::sys::fs::make_absolute(path);

                    for (llvm::StringRef directory = path; !directory.empty(); directory = llvm::sys::path::parent_path(directory)) {
                      if (!llvm::sys::fs::is_directory(directory))
                        continue;

                      auto file = llvm::SmallString< 128 >(directory);
                      llvm::sys::path::append(file, ".swang");

                      DEBUG(llvm::dbgs() << "Trying " << file << "...\n");
                      bool is_file = false;

                      // Ignore errors from is_regular_file: we only need to know if we can read the file or not.
                      llvm::sys::fs::is_regular_file(llvm::Twine(file), is_file);

                      if (!is_file)
                        continue;

                      auto buffer = llvm::MemoryBuffer::getFile(file.c_str());
                      if (buffer.getError()) {
                        llvm::errs() << buffer.getError().message() << "\n";
                        break;
                      }

                      auto error = std::error_code();
                      auto style = cfg::parse(buffer.get()->getBuffer(), error);
                      if (error == std::errc::not_supported)
                        continue;
                      else if (error != std::error_code())
                        break;

                      DEBUG(llvm::dbgs() << "Using configuration file " << file << "\n");
                      return std::move(style);
                    }

                    return swang::config();
                  };

    } // namespace cfg

    auto check = [](llvm::StringRef name, config::style style) {
                   bool matches = true;

                   if (name.startswith(style.prefix))
                     name = name.drop_front(style.prefix.size());
                   else
                     matches = false;

                   if (name.endswith(style.suffix))
                     name = name.drop_back(style.suffix.size());
                   else
                     matches = false;

                   if (!casing::matchers[std::size_t(style.casing)].match(name))
                     matches = false;

                   return matches;
                 };

    auto fixup = [](std::string name, config::style style) {
                   auto words = std::vector< std::string >();

                   auto remaining = name;
                   while (remaining.size() > 0) {
                     auto groups = llvm::SmallVector< llvm::StringRef, 8 >();
                     if (!casing::splitter.match(remaining, &groups))
                       break;

                     if (groups[3].size() > 0)
                     {
                        words.emplace_back(std::move(groups[2]));
                        remaining = remaining.substr(groups[0].size());
                     }
                     else if (groups[4].size() > 0)
                     {
                        words.emplace_back(std::move(groups[4]));
                        remaining = remaining.substr(groups[0].size() - groups[5].size());
                     }
                     else if (groups[6].size() > 0)
                     {
                        words.emplace_back(std::move(groups[6]));
                        remaining = remaining.substr(groups[0].size() - groups[7].size());
                     }
                   }

                   if (words.empty()) {
                     llvm::errs() << "W: unable to split '" << name << "'\n";
                     return style.prefix + name + style.suffix;
                   }

                   auto fixup = std::string(style.prefix);
                   switch (style.casing) {
                     case casing::type::any_case:
                       fixup += name;
                       break;

                     case casing::type::lower_case:
                       for (auto const& word : words) {
                         fixup += llvm::StringRef(word).lower() + "_";
                       }
                       fixup.pop_back();
                       break;

                     case casing::type::upper_case:
                       for (auto const& word : words) {
                         fixup += llvm::StringRef(word).upper() + "_";
                       }
                       fixup.pop_back();
                       break;

                     case casing::type::camel_case:
                       for (auto const& word : words) {
                         auto const w = llvm::StringRef(word);
                         fixup += w.substr(0, 1).upper() + w.substr(1).lower();
                       }
                       break;

                     case casing::type::camel_back:
                       for (auto const& word : words) {
                         auto const w = llvm::StringRef(word);
                         if (&word == &words.front())
                           fixup += w.lower();
                         else
                           fixup += w.substr(0, 1).upper() + w.substr(1).lower();
                       }
                       break;
                   }
                   fixup += style.suffix;

                   return std::move(fixup);
                 };

    class SwangDeclVisitor : public clang::RecursiveASTVisitor< SwangDeclVisitor > {
      public:
        typedef clang::RecursiveASTVisitor< SwangDeclVisitor > base;

        SwangDeclVisitor(clang::tooling::Replacements& replacements) :
          replacements(replacements) {}

        config get_config(clang::Decl* d) {
          auto& context  = d->getASTContext();
          auto& manager  = context.getSourceManager();
          auto  location = clang::FullSourceLoc(d->getLocation(), manager);
          if (d->getLocation().isMacroID()) {
            location = location.getSpellingLoc();
          }

          if (location.getFileID() != this->file_id) {
            auto const file = manager.getFileEntryForID(location.getFileID());
            if (!file)
              return this->cfg;

            this->cfg     = cfg::find(file->getName());
            this->file_id = location.getFileID();
          }

          return this->cfg;
        }

        bool VisitDecl(clang::Decl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          auto diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "swang didn't handle declaration %0");

          diagnostics.Report(clang::FullSourceLoc(d->getLocation(), manager), diagnostic)
            << d->getSourceRange()
            << d->getDeclKindName();

          return true;
        }

        bool VisitNamespaceDecl(clang::NamespaceDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (d->isAnonymousNamespace())
            return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto kindname = "namespace";
          if (false) {
            //
          } else if (d->isInline() && config.inline_namespace_style.is_set) {
            kindname = "inline";
            style    = config.inline_namespace_style;
          } else if (config.namespace_style.is_set) {
            kindname = "namespace";
            style    = config.namespace_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitNamespaceAliasDecl(clang::NamespaceAliasDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto kindname = "namespace";
          style = config.namespace_style;

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitCXXRecordDecl(clang::CXXRecordDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (d->isAnonymousStructOrUnion())
            return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto kindname = "class";
          if (false) {
            //
            // } else if (d->isLambda() && config.lambda_style.is_set) {
            // } else if (d->isInterface() && config.interface_style.is_set) {
          } else if (d->hasDefinition() && d->isAbstract() && config.abstract_style.is_set) {
            kindname = "abstract class";
            style    = config.abstract_style;
          } else if (d->hasDefinition() && d->isPOD() && config.pod_style.is_set) {
            kindname = "POD";
            style    = config.pod_style;
          } else if (d->isStruct() && config.struct_style.is_set) {
            kindname = "struct";
            style    = config.struct_style;
          } else if (d->isStruct() && config.class_style.is_set) {
            kindname = "struct";
            style    = config.class_style;
          } else if (d->isClass() && config.class_style.is_set) {
            kindname = "class";
            style    = config.class_style;
          } else if (d->isClass() && config.struct_style.is_set) {
            kindname = "class";
            style    = config.struct_style;
          } else if (d->isUnion() && config.union_style.is_set) {
            kindname = "union";
            style    = config.union_style;
          } else if (d->isEnum() && config.enum_style.is_set) {
            kindname = "enum";
            style    = config.enum_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitCXXRecordDecl

        bool VisitVarDecl(clang::VarDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto const type     = d->getType();
          auto       kindname = "variable";

          if (false) {

            // } else if (!type.isNull() && type.isLocalRestrictQualified()) {
            // } else if (!type.isNull() && type.isLocalVolatileQualified()) {
            // } else if (!type.isNull() && type.getAsString() == "") {
          } else if (d->isConstexpr() && config.constexpr_variable_style.is_set) {
            kindname = "constexpr";
            style    = config.constexpr_variable_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && d->isFileVarDecl() && config.global_constant_style.is_set) {
            kindname = "global constant";
            style    = config.global_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && d->isStaticDataMember() && config.class_constant_style.is_set) {
            kindname = "class constant";
            style    = config.class_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && d->isLocalVarDecl() && config.local_constant_style.is_set) {
            kindname = "local constant";
            style    = config.local_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && d->isFunctionOrMethodVarDecl() && config.local_constant_style.is_set) {
            kindname = "local constant";
            style    = config.local_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && d->isStaticLocal() && config.static_constant_style.is_set) {
            kindname = "static constant";
            style    = config.static_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && config.constant_style.is_set) {
            kindname = "constant";
            style    = config.constant_style;

            // } else if (d->isWeak()) {
          } else if (d->isFileVarDecl() && config.global_variable_style.is_set) {
            kindname = "global variable";
            style    = config.global_variable_style;
          } else if (d->isLocalVarDecl() && config.local_variable_style.is_set) {
            kindname = "local variable";
            style    = config.local_variable_style;
          } else if (d->isFunctionOrMethodVarDecl() && config.local_variable_style.is_set) {
            kindname = "local variable";
            style    = config.local_variable_style;
          } else if (d->isStaticLocal() && config.static_variable_style.is_set) {
            kindname = "static variable";
            style    = config.static_variable_style;
          } else if (d->isStaticDataMember() && config.class_member_style.is_set) {
            kindname = "class member";
            style    = config.class_member_style;

            // } else if (d->isExceptionVariable() && config.exception_variable_style.is_set) {
            // } else if (d->isNRVOVariable()) {
            // } else if (d->isCXXForRangeDecl()) {
          } else if (config.variable_style.is_set) {
            kindname = "variable";
            style    = config.variable_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitVarDecl

        bool VisitFieldDecl(clang::FieldDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto const type     = d->getType();
          auto       kindname = "member";

          if (false) {

            // } else if (!type.isNull() && type.isLocalRestrictQualified()) {
            // } else if (!type.isNull() && type.isLocalVolatileQualified()) {
            // } else if (!type.isNull() && type.getAsString() == "") {
          } else if (!type.isNull() && type.isLocalConstQualified() && config.member_constant_style.is_set) {
            kindname = "constant member";
            style    = config.member_constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && config.constant_style.is_set) {
            kindname = "constant member";
            style    = config.constant_style;
          } else if (config.member_style.is_set) {
            kindname = "member";
            style    = config.member_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitVarDecl

        bool VisitParmVarDecl(clang::ParmVarDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto const type     = d->getType();
          auto       kindname = "parameter";

          if (false) {

            // } else if (!type.isNull() && type.isLocalRestrictQualified()) {
            // } else if (!type.isNull() && type.isLocalVolatileQualified()) {
            // } else if (!type.isNull() && type.getAsString() == "") {
          } else if (d->isConstexpr() && config.constexpr_variable_style.is_set) {
            kindname = "constexpr";
            style    = config.constexpr_variable_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && config.parameter_constant_style.is_set) {
            kindname = "constant parameter";
            style    = config.constant_style;
          } else if (!type.isNull() && type.isLocalConstQualified() && config.constant_style.is_set) {
            kindname = "constant parameter";
            style    = config.constant_style;
          } else if (d->isParameterPack() && config.parameter_pack_style.is_set) {
            kindname = "parameter pack";
            style    = config.parameter_pack_style;
          } else if (config.parameter_style.is_set) {
            kindname = "parameter";
            style    = config.parameter_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitParmVarDecl

        bool VisitFunctionDecl(clang::FunctionDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          if (d->isMain())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          auto kindname = "parameter";
          if (false) {
            // } else if (d->isVariadic()) {
          } else if (d->isConstexpr() && config.constexpr_function_style.is_set) {
            kindname = "constexpr function";
            style    = config.constexpr_function_style;
          } else if (d->isPure() && config.pure_function_style.is_set) {
            kindname = "pure function";
            style    = config.pure_function_style;

            // } else if (d->isTrivial()) {
            // } else if (d->isVirtualAsWritten()) {
          } else if (d->isGlobal() && config.global_function_style.is_set) {
            kindname = "global function";
            style    = config.global_function_style;

            // } else if (d->isInlineSpecified()) {
            // } else if (d->isOverloadedOperator()) {
            // } else if (d->isFunctionTemplateSpecialization()) {
          } else if (config.function_style.is_set) {
            kindname = "function";
            style    = config.function_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitFunctionDecl

        bool VisitCXXMethodDecl(clang::CXXMethodDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          if (d->isMain())
            return true;
          if (!d->isUserProvided())
            return true;
          if (d->isUsualDeallocationFunction())
            return true;
          if (d->isCopyAssignmentOperator())
            return true;
          if (d->isMoveAssignmentOperator())
            return true;
          if (d->size_overridden_methods() > 0)
            return true;

          auto config   = get_config(d);
          auto style    = config::style();
          auto kindname = "method";
          if (false) {
            // } else if (d->isVariadic()) {
          } else if (d->isConstexpr() && config.constexpr_method_style.is_set) {
            kindname = "constexpr method";
            style    = config.constexpr_method_style;
          } else if (d->isConstexpr() && config.constexpr_function_style.is_set) {
            kindname = "constexpr method";
            style    = config.constexpr_function_style;
          } else if (d->isPure() && config.pure_method_style.is_set) {
            kindname = "pure method";
            style    = config.pure_method_style;
          } else if (d->isPure() && config.pure_function_style.is_set) {
            kindname = "pure method";
            style    = config.pure_function_style;

            // } else if (d->isTrivial()) {
            // } else if (d->isVirtualAsWritten()) {
            // } else if (d->isGlobal()) {
            // } else if (d->isInlineSpecified()) {
            // } else if (d->isOverloadedOperator()) {
            // } else if (d->isFunctionTemplateSpecialization()) {

          } else if (d->isStatic() && config.class_method_style.is_set) {
            kindname = "class method";
            style    = config.class_method_style;

            // } else if (d->isConst()) {
            // } else if (d->isVolatile()) {
          } else if (d->isVirtual() && config.virtual_method_style.is_set) {
            kindname = "virtual method";
            style    = config.virtual_method_style;
          } else if (config.method_style.is_set) {
            kindname = "method";
            style    = config.method_style;
          } else if (config.function_style.is_set) {
            kindname = "method";
            style    = config.function_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitCXXMethodDecl

        bool VisitTypedefDecl(clang::TypedefDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.typedef_style.is_set) {
            style = config.typedef_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = fixup(static_cast< clang::Decl* >(d)->getDeclKindName(), config::style(true, casing::type::lower_case, "", ""));
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        } // VisitTypedefDecl

        bool VisitEnumDecl(clang::EnumDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.enum_style.is_set) {
            style = config.enum_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = fixup(static_cast< clang::Decl* >(d)->getDeclKindName(), config::style(true, casing::type::lower_case, "", ""));
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitEnumConstantDecl(clang::EnumConstantDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.enum_constant_style.is_set) {
            style = config.enum_constant_style;
          } else if (config.constant_style.is_set) {
            style = config.constant_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = fixup(static_cast< clang::Decl* >(d)->getDeclKindName(), config::style(true, casing::type::lower_case, "", ""));
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.type_template_parameter_style.is_set) {
            style = config.type_template_parameter_style;
          } else if (config.template_parameter_style.is_set) {
            style = config.template_parameter_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = "template parameter";
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.value_template_parameter_style.is_set) {
            style = config.value_template_parameter_style;
          } else if (config.template_parameter_style.is_set) {
            style = config.template_parameter_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = "template parameter";
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitTemplateTemplateParmDecl(clang::TemplateTemplateParmDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.template_template_parameter_style.is_set) {
            style = config.template_template_parameter_style;
          } else if (config.template_parameter_style.is_set) {
            style = config.template_parameter_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = "template parameter";
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitTypeAliasDecl(clang::TypeAliasDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.using_style.is_set) {
            style = config.using_style;
          } else if (config.typedef_style.is_set) {
            style = config.typedef_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = "type alias";
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitTypeAliasTemplateDecl(clang::TypeAliasTemplateDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

          if (!d->getIdentifier())
            return true;
          if (d->getName().empty())
            return true;

          auto config = get_config(d);
          auto style  = config::style();

          if (false) {
            //
          } else if (config.template_using_style.is_set) {
            style = config.template_using_style;
          } else if (config.using_style.is_set) {
            style = config.using_style;
          } else if (config.typedef_style.is_set) {
            style = config.typedef_style;
          }

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name        = d->getName();
          auto const range       = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname    = "type alias";
          auto const replacement = fixup(name, style);
          auto const diagnostic  = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                               "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, replacement) << range;
          replacements.insert(clang::tooling::Replacement(manager, clang::CharSourceRange::getCharRange(range), replacement));

          return true;
        }

        bool VisitClassScopeFunctionSpecializationDecl(clang::ClassScopeFunctionSpecializationDecl* d) {
          return VisitCXXMethodDecl(d->getSpecialization());
        }

        bool VisitAccessSpecDecl(clang::AccessSpecDecl* d) { return true; }
        bool VisitClassTemplateDecl(clang::ClassTemplateDecl* d) { return true; }
        bool VisitClassTemplatePartialSpecializationDecl(clang::ClassTemplatePartialSpecializationDecl* d) { return true; }
        bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl* d) { return true; }
        bool VisitCXXConstructorDecl(clang::CXXConstructorDecl* d) { return true; }
        bool VisitCXXConversionDecl(clang::CXXConversionDecl* d) { return true; }
        bool VisitCXXDestructorDecl(clang::CXXDestructorDecl* d) { return true; }
        bool VisitEmptyDecl(clang::EmptyDecl* d) { return true; }
        bool VisitFriendDecl(clang::FriendDecl* d) { return true; }
        bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* d) { return true; }
        bool VisitLinkageSpecDecl(clang::LinkageSpecDecl* d) { return true; }
        bool VisitStaticAssertDecl(clang::StaticAssertDecl* d) { return true; }
        bool VisitTranslationUnitDecl(clang::TranslationUnitDecl* d) { return true; }
        bool VisitUnresolvedUsingValueDecl(clang::UnresolvedUsingValueDecl* d) { return true; }
        bool VisitUsingDecl(clang::UsingDecl* d) { return true; }
        bool VisitUsingDirectiveDecl(clang::UsingDirectiveDecl* d) { return true; }

        bool VisitBlockDecl(clang::BlockDecl* d) { return VisitDecl(d); }
        bool VisitCapturedDecl(clang::CapturedDecl* d) { return VisitDecl(d); }
        bool VisitDeclaratorDecl(clang::DeclaratorDecl* d) { return VisitDecl(d); }
        bool VisitFileScopeAsmDecl(clang::FileScopeAsmDecl* d) { return VisitDecl(d); }
        bool VisitFriendTemplateDecl(clang::FriendTemplateDecl* d) { return VisitDecl(d); }
        bool VisitImplicitParamDecl(clang::ImplicitParamDecl* d) { return VisitDecl(d); }
        bool VisitImportDecl(clang::ImportDecl* d) { return VisitDecl(d); }
        bool VisitIndirectFieldDecl(clang::IndirectFieldDecl* d) { return VisitDecl(d); }
        bool VisitLabelDecl(clang::LabelDecl* d) { return VisitDecl(d); }
        bool VisitMSPropertyDecl(clang::MSPropertyDecl* d) { return VisitDecl(d); }
        bool VisitNamedDecl(clang::NamedDecl* d) { return VisitDecl(d); }
        bool VisitOMPThreadPrivateDecl(clang::OMPThreadPrivateDecl* d) { return VisitDecl(d); }
        bool VisitRecordDecl(clang::RecordDecl* d) { return VisitDecl(d); }
        bool VisitRedeclarableTemplateDecl(clang::RedeclarableTemplateDecl* d) { return VisitDecl(d); }
        bool VisitTagDecl(clang::TagDecl* d) { return VisitDecl(d); }
        bool VisitTemplateDecl(clang::TemplateDecl* d) { return VisitDecl(d); }
        bool VisitTypeDecl(clang::TypeDecl* d) { return VisitDecl(d); }
        bool VisitTypedefNameDecl(clang::TypedefNameDecl* d) { return VisitDecl(d); }
        bool VisitUsingShadowDecl(clang::UsingShadowDecl* d) { return VisitDecl(d); }
        bool VisitValueDecl(clang::ValueDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplateDecl(clang::VarTemplateDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplatePartialSpecializationDecl(clang::VarTemplatePartialSpecializationDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplateSpecializationDecl(clang::VarTemplateSpecializationDecl* d) { return VisitDecl(d); }

        bool VisitObjCAtDefsFieldDecl(clang::ObjCAtDefsFieldDecl* d) { return VisitDecl(d); }
        bool VisitObjCCategoryDecl(clang::ObjCCategoryDecl* d) { return VisitDecl(d); }
        bool VisitObjCCategoryImplDecl(clang::ObjCCategoryImplDecl* d) { return VisitDecl(d); }
        bool VisitObjCCompatibleAliasDecl(clang::ObjCCompatibleAliasDecl* d) { return VisitDecl(d); }
        bool VisitObjCContainerDecl(clang::ObjCContainerDecl* d) { return VisitDecl(d); }
        bool VisitObjCImplDecl(clang::ObjCImplDecl* d) { return VisitDecl(d); }
        bool VisitObjCImplementationDecl(clang::ObjCImplementationDecl* d) { return VisitDecl(d); }
        bool VisitObjCInterfaceDecl(clang::ObjCInterfaceDecl* d) { return VisitDecl(d); }
        bool VisitObjCIvarDecl(clang::ObjCIvarDecl* d) { return VisitDecl(d); }
        bool VisitObjCMethodDecl(clang::ObjCMethodDecl* d) { return VisitDecl(d); }
        bool VisitObjCPropertyDecl(clang::ObjCPropertyDecl* d) { return VisitDecl(d); }
        bool VisitObjCPropertyImplDecl(clang::ObjCPropertyImplDecl* d) { return VisitDecl(d); }
        bool VisitObjCProtocolDecl(clang::ObjCProtocolDecl* d) { return VisitDecl(d); }

        // Declare WalkUpFrom*() for all concrete Decl classes.
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)                                      \
  bool WalkUpFrom ## CLASS ## Decl(clang::CLASS ## Decl * d) { \
    return Visit ## CLASS ## Decl(d);                          \
  } // namespace {
#include "clang/AST/DeclNodes.inc"

        // The above header #undefs ABSTRACT_DECL and DECL upon exit.

        // Declare Traverse*() for all concrete Decl classes.
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)                                    \
  bool Traverse ## CLASS ## Decl(clang::CLASS ## Decl * d) { \
    return base::Traverse ## CLASS ## Decl(d);               \
  }
#include "clang/AST/DeclNodes.inc"

        // The above header #undefs ABSTRACT_DECL and DECL upon exit.

      private:
        clang::tooling::Replacements& replacements;
        swang::config cfg;
        clang::FileID file_id;

        std::string tag;
        std::string name;
        std::set< std::string > attribs;
        std::set< std::string > classes;

        std::vector< std::string > tags;
    };

    class SwangASTConsumer : public clang::ASTConsumer {
      public:
        SwangASTConsumer(clang::tooling::Replacements& replacements) :
          visitor(replacements) {}

        virtual void HandleTranslationUnit(clang::ASTContext& context) {
          visitor.TraverseDecl(context.getTranslationUnitDecl());
        }

      private:
        SwangDeclVisitor visitor;
    };

    class SwangAction {
      public:
        SwangAction(clang::tooling::Replacements& replacements) :
          replacements(replacements) {}

        std::unique_ptr< clang::ASTConsumer > newASTConsumer() { return llvm::make_unique< SwangASTConsumer >(replacements); }

      private:
        clang::tooling::Replacements& replacements;
    };

  } // end anonymous namespace
} // namespace swang

auto build_path   = llvm::cl::opt< std::string >(llvm::cl::Positional, llvm::cl::desc("<build-path>"));
auto source_paths = llvm::cl::list< std::string >(llvm::cl::Positional, llvm::cl::desc("<source0> [... <sourceN>]"), llvm::cl::OneOrMore);

int main(int argc, const char** argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  auto database = std::unique_ptr< clang::tooling::CompilationDatabase >(clang::tooling::FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  llvm::cl::ParseCommandLineOptions(argc, argv);

  if (!database) {
    auto error_message = std::string();
    database = !build_path.empty()
               ? clang::tooling::CompilationDatabase::autoDetectFromDirectory(build_path, error_message)
               : clang::tooling::CompilationDatabase::autoDetectFromSource(source_paths[0], error_message);

    if (!database)
      llvm::report_fatal_error(error_message);
  }

  auto tool   = clang::tooling::RefactoringTool(*database, source_paths);
  auto action = swang::SwangAction(tool.getReplacements());

  return tool.run(clang::tooling::newFrontendActionFactory(&action).get());
}
