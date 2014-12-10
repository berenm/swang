#include "swang_config.hpp"

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/YAMLTraits.h"

namespace {

  static constexpr char const* const casing_names[] = {
    "aNy_CasE",
    "lower_case",
    "UPPER_CASE",
    "CamelCase",
    "camelBack",
  };

  static llvm::Regex casing_matchers[] = {
    llvm::Regex { llvm::StringRef { ".*" } },
    llvm::Regex { llvm::StringRef { "[a-z][a-z0-9_]*" } },
    llvm::Regex { llvm::StringRef { "[A-Z][A-Z0-9_]*" } },
    llvm::Regex { llvm::StringRef { "[A-Z]+[a-z][a-zA-Z0-9]*" } },
    llvm::Regex { llvm::StringRef { "[a-z][a-zA-Z0-9]*" } },
  };

  static llvm::Regex casing_splitters[] = {
    llvm::Regex { llvm::StringRef { "(.*)" } },
    llvm::Regex { llvm::StringRef { "([a-z]+)(?:_|$)" } },
    llvm::Regex { llvm::StringRef { "([A-Z]+)(?:_|$)" } },
    llvm::Regex { llvm::StringRef { "([A-Z]+?|[A-Z][a-z]+)" } },
    llvm::Regex { llvm::StringRef { "([a-zA-Z][a-z0-9]*)" } },
  };

  class SwangCallback : public clang::ast_matchers::MatchFinder::MatchCallback {
    public:
      SwangCallback(clang::tooling::Replacements* replacements) : replacements(replacements) {}

      void run(clang::ast_matchers::MatchFinder::MatchResult const & result) override {
        auto& source_manager = *result.SourceManager;
        auto& diagnostics    = result.Context->getDiagnostics();

        auto check = [](llvm::StringRef name, swang::config::style style) {
                       bool matches = true;

                       if (name.startswith(style.prefix))
                         name.drop_front(style.prefix.size());
                       else
                         matches = false;

                       if (name.endswith(style.suffix))
                         name.drop_back(style.suffix.size());
                       else
                         matches = false;

                       if (!casing_matchers[std::size_t { style.casing }].match(name))
                         matches = false;

                       return std::make_tuple(matches, name);
                     };

        auto fixup = [](std::string name, swang::config::style style) {
                       llvm::SmallVector<llvm::StringRef, 8> words;

                       for (auto& matcher : casing_matchers) {
                         if (!matcher.match(name))
                           continue;

                         casing_splitters[&matcher - &casing_matchers[0]].match(name, &words);
                         words.pop_back();
                       }

                       auto fixup = std::string { style.prefix };
                       switch (style.casing) {
                         case swang::casing_type::any_case:
                           break;

                         case swang::casing_type::lower_case:
                           for (auto const& word : words) { fixup += word.lower() + "_"; }
                           fixup.pop_back();
                           break;

                         case swang::casing_type::upper_case:
                           for (auto const& word : words) { fixup += word.upper() + "_"; }
                           fixup.pop_back();
                           break;

                         case swang::casing_type::camel_case:
                           for (auto const& word : words) { fixup += word.substr(0, 1).upper() + word.substr(1).lower(); }
                           break;

                         case swang::casing_type::camel_back:
                           for (auto const& word : words) {
                             if (&word == &words.front())
                               fixup += word.lower();
                             else
                               fixup += word.substr(0, 1).upper() + word.substr(1).lower();
                           }
                           break;
                       }
                       fixup += style.suffix;

                       return std::move(fixup);
                     };

        auto report = [&](clang::CharSourceRange source_range, unsigned diagnostic, std::string name, swang::config::style style) {
                        diagnostics.Report(clang::FullSourceLoc(source_range.getBegin(), source_manager), diagnostic)
                          << name << style.prefix << casing_names[std::size_t { style.casing }] << style.suffix
                          << clang::FixItHint::CreateReplacement(source_range, fixup(name, style))
                          << source_range;
                      };

        if (!config_initialized) {
          auto file_name = source_manager.getFileEntryForID(source_manager.getMainFileID())->getName();
          config             = swang::get_config("file", file_name, "default");
          config_initialized = true;
        }

        if (auto decl = result.Nodes.getNodeAs<clang::NamespaceDecl>("namespace")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isAnonymousNamespace())
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::RecordDecl>("record")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isAnonymousStructOrUnion())
            return;

          if (decl->isStruct())
            ;
          else if (decl->isClass())
            ;
          else if (decl->isUnion())
            ;

        } else if (auto decl = result.Nodes.getNodeAs<clang::ClassTemplateDecl>("template")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::ClassTemplateSpecializationDecl>("specialization")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::DeclaratorDecl>("declarator")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::ParmVarDecl>("parameter")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isParameterPack())
            ;
          else
            ;

          auto const qualified_type = decl->getOriginalType();

        } else if (auto decl = result.Nodes.getNodeAs<clang::AccessSpecDecl>("access")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::CXXConstructorDecl>("constructor")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::CXXDestructorDecl>("destructor")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::EnumDecl>("enum")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isScoped())
            ;
          else
            ;

        } else if (auto decl = result.Nodes.getNodeAs<clang::EnumConstantDecl>("constant")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::CXXConversionDecl>("method")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (!decl->isUserProvided())
            return;

          if (decl->isConst())
            ;
          else
            ;

          if (decl->isVolatile())
            ;
          else
            ;

          if (decl->isVirtual())
            ;
          else
            ;

          if (decl->isExplicit())
            ;
          else
            ;
        } else if (auto decl = result.Nodes.getNodeAs<clang::CXXMethodDecl>("method")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (!decl->isUserProvided())
            return;

          if (decl->isUsualDeallocationFunction())
            ;
          else if (decl->isCopyAssignmentOperator())
            ;
          else if (decl->isMoveAssignmentOperator())
            ;
          else
            ;

          if (decl->isStatic())
            ;
          else
            ;

          if (decl->isConst())
            ;
          else
            ;

          if (decl->isVolatile())
            ;
          else
            ;

          if (decl->isVirtual())
            ;
          else
            ;

        } else if (auto decl = result.Nodes.getNodeAs<clang::VarDecl>("variable")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::FieldDecl>("field")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::FunctionDecl>("function")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::FunctionTemplateDecl>("functionTemplate")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::FriendDecl>("friend")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::UsingDecl>("using")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::UnresolvedUsingValueDecl>("unresolved")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::NamedDecl>("named")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else if (auto decl = result.Nodes.getNodeAs<clang::Decl>("decl")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

        } else {}

        if (auto decl = result.Nodes.getNodeAs<clang::NamespaceDecl>("namespace")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isAnonymousNamespace())
            return;

          auto style = swang::config::style {};
          if (decl->isInline() && config.inline_namespace_style.is_set)
            style = config.inline_namespace_style;
          else if (config.namespace_style.is_set)
            style = config.namespace_style;

          if (!style.is_set)
            return;

          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "inline namespace '%0' hasn't got swag like '%1%2%3'.");
          auto       name       = std::string {};
          auto       matches = true;
          std::tie(matches, name) = check(decl->getName().str(), style);

          auto const source     = clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(decl->getSourceRange()), source_manager, clang::LangOptions());
          auto const nameOffset = source.find(name, source.find("namespace") + std::strlen("namespace"));
          auto const nameStart  = decl->getLocStart().getLocWithOffset(nameOffset);
          auto const nameEnd    = decl->getLocStart().getLocWithOffset(nameOffset + name.size() - 1);
          auto const nameRange  = clang::CharSourceRange::getTokenRange(nameStart, nameEnd);

          report(nameRange, diagnostic, name, style);

        } else if (auto decl = result.Nodes.getNodeAs<clang::RecordDecl>("record")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isAnonymousStructOrUnion())
            return;

          if (decl->isStruct())
            ;
          else if (decl->isClass())
            ;
          else if (decl->isUnion())
            ;

          auto style = swang::config::style {};
          if (decl->isInline() && config.inline_namespace_style.is_set)
            style = config.inline_namespace_style;
          else if (config.namespace_style.is_set)
            style = config.namespace_style;

          if (!style.is_set)
            return;

          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "inline namespace '%0' hasn't got swag like '%1%2%3'.");
          auto       name       = std::string {};
          auto       matches = true;
          std::tie(matches, name) = check(decl->getName().str(), style);

          auto const source     = clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(decl->getSourceRange()), source_manager, clang::LangOptions());
          auto const nameOffset = source.find(name, source.find("namespace") + std::strlen("namespace"));
          auto const nameStart  = decl->getLocStart().getLocWithOffset(nameOffset);
          auto const nameEnd    = decl->getLocStart().getLocWithOffset(nameOffset + name.size() - 1);
          auto const nameRange  = clang::CharSourceRange::getTokenRange(nameStart, nameEnd);

          report(nameRange, diagnostic, name, style);

        } else if (auto decl = result.Nodes.getNodeAs<clang::NamedDecl>("named")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          auto diagnotic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "invalid declaration %0");
          diagnostics.Report(clang::FullSourceLoc(decl->getLocation(), source_manager), diagnotic)
            << decl->getSourceRange()
            << clang::FixItHint::CreateReplacement(decl->getSourceRange(), "named")
            << decl->getDeclKindName();

        } else if (auto decl = result.Nodes.getNodeAs<clang::Decl>("decl")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          auto diagnotic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Error, "invalid declaration %0");
          diagnostics.Report(clang::FullSourceLoc(decl->getLocation(), source_manager), diagnotic)
            << decl->getSourceRange()
            << clang::FixItHint::CreateReplacement(decl->getSourceRange(), "coucou")
            << decl->getDeclKindName();
        }
      }

      void onStartOfTranslationUnit() override { config_initialized = false; }

      void onEndOfTranslationUnit() override {}

    private:
      clang::tooling::Replacements* replacements;
      swang::config                 config;
      bool                          config_initialized = false;
  };

} // end anonymous namespace

auto build_path = llvm::cl::opt<std::string> { llvm::cl::Positional, llvm::cl::desc("<build-path>") };
auto source_paths = llvm::cl::list<std::string> { llvm::cl::Positional, llvm::cl::desc("<source0> [... <sourceN>]"), llvm::cl::OneOrMore };

int main(int argc, const char** argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  auto database = std::unique_ptr<clang::tooling::CompilationDatabase>(clang::tooling::FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  llvm::cl::ParseCommandLineOptions(argc, argv);

  if (!database) {
    auto error_message = std::string {};
    database.reset(!build_path.empty()
                   ? clang::tooling::CompilationDatabase::autoDetectFromDirectory(build_path, error_message)
                   : clang::tooling::CompilationDatabase::autoDetectFromSource(source_paths[0], error_message)
                   );

    if (!database)
      llvm::report_fatal_error(error_message);
  }

  auto tool   = clang::tooling::RefactoringTool { *database, source_paths };
  auto finder = clang::ast_matchers::MatchFinder {};
  auto callback = SwangCallback { &tool.getReplacements() };

  auto declMatcher = clang::ast_matchers::anyOf(
    clang::ast_matchers::anyOf(
      clang::ast_matchers::namespaceDecl().bind("namespace"),
      clang::ast_matchers::recordDecl().bind("record"),
      clang::ast_matchers::classTemplateDecl().bind("template"),
      clang::ast_matchers::classTemplateSpecializationDecl().bind("specialization"),
      clang::ast_matchers::declaratorDecl().bind("declarator")),
    clang::ast_matchers::anyOf(
      clang::ast_matchers::parmVarDecl().bind("parameter"),
      clang::ast_matchers::accessSpecDecl().bind("access"),
      clang::ast_matchers::constructorDecl().bind("constructor"),
      clang::ast_matchers::destructorDecl().bind("destructor"),
      clang::ast_matchers::enumDecl().bind("enum")),
    clang::ast_matchers::anyOf(
      clang::ast_matchers::enumConstantDecl().bind("constant"),
      clang::ast_matchers::methodDecl().bind("method"),
      clang::ast_matchers::varDecl().bind("variable"),
      clang::ast_matchers::fieldDecl().bind("field"),
      clang::ast_matchers::functionDecl().bind("function")),
    clang::ast_matchers::anyOf(
      clang::ast_matchers::functionTemplateDecl().bind("functionTemplate"),
      clang::ast_matchers::friendDecl().bind("friend"),
      clang::ast_matchers::usingDecl().bind("using"),
      clang::ast_matchers::unresolvedUsingValueDecl().bind("unresolved"),
      clang::ast_matchers::namedDecl().bind("named")),
    clang::ast_matchers::decl().bind("decl"));

  finder.addMatcher(clang::ast_matchers::decl(declMatcher), &callback);
  finder.addMatcher(clang::ast_matchers::declRefExpr(clang::ast_matchers::hasDeclaration(declMatcher)), &callback);

  return tool.run(clang::tooling::newFrontendActionFactory(&finder).get());
}
