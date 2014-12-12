#include "swang_config.hpp"

#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
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
#include "llvm/Support/Regex.h"

#include <sstream>

namespace swang {

  namespace casing {
    static char const* const names[] = {
      "aNy_CasE",
      "lower_case",
      "camelBack",
      "UPPER_CASE",
      "CamelCase",
    };

    static llvm::Regex matchers[] = {
      llvm::Regex(llvm::StringRef(".*")),
      llvm::Regex(llvm::StringRef("[a-z][a-z0-9_]*")),
      llvm::Regex(llvm::StringRef("[a-z][a-zA-Z0-9]*")),
      llvm::Regex(llvm::StringRef("[A-Z][A-Z0-9_]*")),
      llvm::Regex(llvm::StringRef("[A-Z]+[a-z][a-zA-Z0-9]*")),
    };

    static llvm::Regex splitters[] = {
      llvm::Regex(llvm::StringRef("^(.*)")),
      llvm::Regex(llvm::StringRef("^([a-z]+)(_|$)")),
      llvm::Regex(llvm::StringRef("^([a-zA-Z][a-z0-9]*)")),
      llvm::Regex(llvm::StringRef("^([A-Z]+)(_|$)")),
      llvm::Regex(llvm::StringRef("^([A-Z]+|[A-Z][a-z]+)")),
    };
  }

  namespace {

    auto const check = [](llvm::StringRef name, config::style style) {
                         bool matches = true;

                         if (name.startswith(style.prefix))
                           name.drop_front(style.prefix.size());
                         else
                           matches = false;

                         if (name.endswith(style.suffix))
                           name.drop_back(style.suffix.size());
                         else
                           matches = false;

                         if (!casing::matchers[std::size_t(style.casing)].match(name))
                           matches = false;

                         return matches;
                       };

    auto const split = [](std::string name, casing::type casing, std::vector< std::string >& words) {
                         auto& matcher  = casing::matchers[casing];
                         auto& splitter = casing::splitters[casing];

                         if (!matcher.match(name))
                           return false;

                         auto remaining = name;
                         while (remaining.size() > 0) {
                           auto groups = llvm::SmallVector< llvm::StringRef, 8 >();
                           if (!splitter.match(remaining, &groups))
                             return false;

                           auto const word = groups[1];
                           if ((casing::names[casing] == std::string("CamelCase"))
                               && (word.size() > 1) && std::isupper(word.back())) {
                             remaining = remaining.substr(word.size() - 1);
                             words.push_back(word.substr(0, word.size() - 1));
                           } else {
                             remaining = remaining.substr(groups[0].size());
                             words.push_back(std::move(word));
                           }
                         }

                         return true;
                       };

    auto const fixup = [](std::string name, config::style style) {
                         auto words = std::vector< std::string >();

                         for (auto& matcher : casing::matchers) {
                           auto splits = std::vector< std::string >();
                           if (!split(name, casing::type(&matcher - &casing::matchers[0]), splits))
                             continue;

                           words = std::move(splits);
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

          if (location.getFileID() != this->file_id) {
            auto const file = manager.getFileEntryForID(location.getFileID());
            if (!file)
              return this->cfg;

            auto const file_name = file->getName();
            this->cfg     = swang::get_config("file", file_name, "default");
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

          auto diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "invalid declaration %0");

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

          auto config = get_config(d);
          auto style  = config::style();

          if (d->isInline() && config.inline_namespace_style.is_set)
            style = config.inline_namespace_style;
          else if (config.namespace_style.is_set)
            style = config.namespace_style;

          if (!style.is_set)
            return true;

          if (check(d->getName(), style))
            return true;

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname   = fixup(static_cast< clang::Decl* >(d)->getDeclKindName(), config::style(true, casing::type::lower_case, "", ""));
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

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

          auto config = get_config(d);
          auto style  = config::style();

          auto kindname = "class";
          if (false) {
            //
          } else if (d->isLambda() && config.lambda_style.is_set) {
            kindname = "lambda";
            style    = config.lambda_style;
          } else if (d->isInterface() && config.interface_style.is_set) {
            kindname = "interface";
            style    = config.interface_style;
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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitCXXRecordDecl

        bool VisitVarDecl(clang::VarDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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
          } else if (d->isExceptionVariable() && config.exception_variable_style.is_set) {
            kindname = "exception variable";
            style    = config.exception_variable_style;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitVarDecl

        bool VisitFieldDecl(clang::FieldDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitVarDecl

        bool VisitParmVarDecl(clang::ParmVarDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitParmVarDecl

        bool VisitFunctionDecl(clang::FunctionDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitFunctionDecl

        bool VisitCXXMethodDecl(clang::CXXMethodDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitCXXMethodDecl

        bool VisitTypedefDecl(clang::TypedefDecl* d) {
          auto& context     = d->getASTContext();
          auto& manager     = context.getSourceManager();
          auto& diagnostics = context.getDiagnostics();

          // if (!manager.isWrittenInMainFile(d->getLocation()))
          // return true;

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

          auto const name       = d->getName();
          auto const range      = clang::DeclarationNameInfo(d->getDeclName(), d->getLocation()).getSourceRange();
          auto const kindname   = fixup(static_cast< clang::Decl* >(d)->getDeclKindName(), config::style(true, casing::type::lower_case, "", ""));
          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                              "%0 '%1' doesn't swag like '%2%3%4'.");

          diagnostics.Report(clang::FullSourceLoc(range.getBegin(), manager), diagnostic)
            << kindname << d->getName() << style.prefix << casing::names[std::size_t(style.casing)] << style.suffix
            << clang::FixItHint::CreateReplacement(range, fixup(name, style))
            << range;

          return true;
        } // VisitTypedefDecl

        bool VisitEnumDecl(clang::EnumDecl* d) { return true; }
        bool VisitEnumConstantDecl(clang::EnumConstantDecl* d) { return true; }

        bool VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl* d) { return true; }
        bool VisitNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl* d) { return true; }
        bool VisitTemplateTemplateParmDecl(clang::TemplateTemplateParmDecl* d) { return true; }

        bool VisitClassTemplateDecl(clang::ClassTemplateDecl* d) { return true; }
        bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl* d) { return true; }
        bool VisitClassTemplatePartialSpecializationDecl(clang::ClassTemplatePartialSpecializationDecl* d) { return true; }

        bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* d) { return true; }

        bool VisitTypeAliasDecl(clang::TypeAliasDecl* d) { return true; }
        bool VisitTypeAliasTemplateDecl(clang::TypeAliasTemplateDecl* d) { return true; }

        bool VisitFriendDecl(clang::FriendDecl* d) { return true; }
        bool VisitAccessSpecDecl(clang::AccessSpecDecl* d) { return true; }
        bool VisitBlockDecl(clang::BlockDecl* d) { return VisitDecl(d); }
        bool VisitCapturedDecl(clang::CapturedDecl* d) { return VisitDecl(d); }
        bool VisitClassScopeFunctionSpecializationDecl(clang::ClassScopeFunctionSpecializationDecl* d) { return VisitDecl(d); }
        bool VisitCXXConstructorDecl(clang::CXXConstructorDecl* d) { return true; }
        bool VisitCXXConversionDecl(clang::CXXConversionDecl* d) { return true; }
        bool VisitCXXDestructorDecl(clang::CXXDestructorDecl* d) { return true; }
        bool VisitDeclaratorDecl(clang::DeclaratorDecl* d) { return VisitDecl(d); }
        bool VisitEmptyDecl(clang::EmptyDecl* d) { return true; }
        bool VisitFileScopeAsmDecl(clang::FileScopeAsmDecl* d) { return VisitDecl(d); }
        bool VisitFriendTemplateDecl(clang::FriendTemplateDecl* d) { return VisitDecl(d); }
        bool VisitImplicitParamDecl(clang::ImplicitParamDecl* d) { return VisitDecl(d); }
        bool VisitImportDecl(clang::ImportDecl* d) { return VisitDecl(d); }
        bool VisitIndirectFieldDecl(clang::IndirectFieldDecl* d) { return VisitDecl(d); }
        bool VisitLabelDecl(clang::LabelDecl* d) { return VisitDecl(d); }
        bool VisitLinkageSpecDecl(clang::LinkageSpecDecl* d) { return true; }
        bool VisitMSPropertyDecl(clang::MSPropertyDecl* d) { return VisitDecl(d); }
        bool VisitNamedDecl(clang::NamedDecl* d) { return VisitDecl(d); }
        bool VisitNamespaceAliasDecl(clang::NamespaceAliasDecl* d) { return VisitDecl(d); }
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
        bool VisitOMPThreadPrivateDecl(clang::OMPThreadPrivateDecl* d) { return VisitDecl(d); }
        bool VisitRecordDecl(clang::RecordDecl* d) { return VisitDecl(d); }
        bool VisitRedeclarableTemplateDecl(clang::RedeclarableTemplateDecl* d) { return VisitDecl(d); }
        bool VisitStaticAssertDecl(clang::StaticAssertDecl* d) { return true; }
        bool VisitTagDecl(clang::TagDecl* d) { return VisitDecl(d); }
        bool VisitTemplateDecl(clang::TemplateDecl* d) { return VisitDecl(d); }
        bool VisitTranslationUnitDecl(clang::TranslationUnitDecl* d) { return true; }
        bool VisitTypeDecl(clang::TypeDecl* d) { return VisitDecl(d); }
        bool VisitTypedefNameDecl(clang::TypedefNameDecl* d) { return VisitDecl(d); }
        bool VisitUnresolvedUsingValueDecl(clang::UnresolvedUsingValueDecl* d) { return true; }
        bool VisitUsingDecl(clang::UsingDecl* d) { return true; }
        bool VisitUsingDirectiveDecl(clang::UsingDirectiveDecl* d) { return true; }
        bool VisitUsingShadowDecl(clang::UsingShadowDecl* d) { return VisitDecl(d); }
        bool VisitValueDecl(clang::ValueDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplateDecl(clang::VarTemplateDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplatePartialSpecializationDecl(clang::VarTemplatePartialSpecializationDecl* d) { return VisitDecl(d); }
        bool VisitVarTemplateSpecializationDecl(clang::VarTemplateSpecializationDecl* d) { return VisitDecl(d); }

        // Declare WalkUpFrom*() for all concrete Decl classes.
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)                                      \
  bool WalkUpFrom ## CLASS ## Decl(clang::CLASS ## Decl * d) { \
    auto r = Visit ## CLASS ## Decl(d);                        \
    push();                                                    \
    return r;                                                  \
  } // namespace {
#include "clang/AST/DeclNodes.inc"

        // The above header #undefs ABSTRACT_DECL and DECL upon exit.

        // Declare Traverse*() for all concrete Decl classes.
#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)                                    \
  bool Traverse ## CLASS ## Decl(clang::CLASS ## Decl * d) { \
    auto r = base::Traverse ## CLASS ## Decl(d);             \
    pop();                                                   \
    return r;                                                \
  }
#include "clang/AST/DeclNodes.inc"

        // The above header #undefs ABSTRACT_DECL and DECL upon exit.

        void push() {}
        void pop() {}

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
          replacements(replacements),
          visitor(replacements) {}

        virtual void HandleTranslationUnit(clang::ASTContext& context) {
          // Traversing the translation unit decl via a RecursiveASTVisitor
          // will visit all nodes in the AST.
          visitor.TraverseDecl(context.getTranslationUnitDecl());
        }

      private:
        clang::tooling::Replacements& replacements;
        SwangDeclVisitor              visitor;
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
  auto finder = swang::SwangAction(tool.getReplacements());

  return tool.run(clang::tooling::newFrontendActionFactory(&finder).get());
}
