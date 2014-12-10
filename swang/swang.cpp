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
#include "llvm/Support/Regex.h"

#include <sstream>

namespace {

  char const* const casing_names[] = {
    "aNy_CasE",
    "lower_case",
    "UPPER_CASE",
    "CamelCase",
    "camelBack",
  };

  static llvm::Regex casing_matchers[] = {
    llvm::Regex(llvm::StringRef(".*")),
    llvm::Regex(llvm::StringRef("[a-z][a-z0-9_]*")),
    llvm::Regex(llvm::StringRef("[A-Z][A-Z0-9_]*")),
    llvm::Regex(llvm::StringRef("[A-Z]+[a-z][a-zA-Z0-9]*")),
    llvm::Regex(llvm::StringRef("[a-z][a-zA-Z0-9]*")),
  };

  static llvm::Regex casing_splitters[] = {
    llvm::Regex(llvm::StringRef("(.*)")),
    llvm::Regex(llvm::StringRef("([a-z]+)(?:_|$)")),
    llvm::Regex(llvm::StringRef("([A-Z]+)(?:_|$)")),
    llvm::Regex(llvm::StringRef("([A-Z]+?|[A-Z][a-z]+)")),
    llvm::Regex(llvm::StringRef("([a-zA-Z][a-z0-9]*)")),
  };

  class SwangCallback : public clang::ast_matchers::MatchFinder::MatchCallback {
    public:
      SwangCallback(clang::tooling::Replacements* replacements) : replacements(replacements), config_initialized(false) {}

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

                       if (!casing_matchers[std::size_t(style.casing)].match(name))
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

                       auto fixup = std::string(style.prefix);
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
                          << name << style.prefix << casing_names[std::size_t(style.casing)] << style.suffix
                          << clang::FixItHint::CreateReplacement(source_range, fixup(name, style))
                          << source_range;
                      };

        if (!config_initialized) {
          auto file_name = source_manager.getFileEntryForID(source_manager.getMainFileID())->getName();
          config             = swang::get_config("file", file_name, "default");
          config_initialized = true;
        }

        std::string           tag     = "";
        std::string           name    = "";
        std::set<std::string> attribs;
        std::set<std::string> classes;
        if (auto d = result.Nodes.getNodeAs<clang::Decl>("decl")) {
          if (!source_manager.isWrittenInMainFile(d->getLocation()))
            return;

          llvm::errs() << clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(d->getSourceRange()), source_manager, clang::LangOptions()) << "\n";

          if (auto const decl = clang::dyn_cast<clang::Decl>(d)) {
            tag = "decl";

            switch (decl->getAccess()) {
              case clang::AS_public:
                attribs.insert("public");
                break;

              case clang::AS_protected:
                attribs.insert("protected");
                break;

              case clang::AS_private:
                attribs.insert("private");
                break;

              case clang::AS_none:
                break;
            }

            if (decl->isDeprecated())
              attribs.insert("deprecated");

            if (decl->isUnavailable())
              attribs.insert("unavailable");

            if (decl->isParameterPack())
              classes.insert("pack");

            if (auto const decl = clang::dyn_cast<clang::NamedDecl>(d)) {
              tag  = "named";
              name = decl->getName();

              if (auto const decl = clang::dyn_cast<clang::LabelDecl>(d)) {
                tag = "label";

                if (decl->isGnuLocal())
                  attribs.insert("scope=\"local\"");

              } else if (auto const decl = clang::dyn_cast<clang::NamespaceAliasDecl>(d)) {
                tag = "namespace";
                classes.insert("alias");

              } else if (auto const decl = clang::dyn_cast<clang::NamespaceDecl>(d)) {
                tag = "namespace";

                if (decl->isAnonymousNamespace())
                  return;

                if (decl->isInline())
                  attribs.insert("inline");

              } else if (auto const decl = clang::dyn_cast<clang::TemplateDecl>(d)) {
                classes.insert("template");

                if (auto const decl = clang::dyn_cast<clang::RedeclarableTemplateDecl>(d)) {
                  classes.insert("redeclarable");

                  if (decl->isMemberSpecialization()) {
                    classes.insert("member");
                    classes.insert("specialization");
                  }

                  if (auto const decl = clang::dyn_cast<clang::ClassTemplateDecl>(d))
                    tag = "class";
                  else if (auto const decl = clang::dyn_cast<clang::FunctionTemplateDecl>(d))
                    tag = "function";
                  else if (auto const decl = clang::dyn_cast<clang::TypeAliasTemplateDecl>(d))
                    tag = "type-alias";
                  else if (auto const decl = clang::dyn_cast<clang::VarTemplateDecl>(d))
                    tag = "variable";
                  else
                    llvm_unreachable("Unknown RedeclarableTemplate declaration!");

                } else if (auto const decl = clang::dyn_cast<clang::TemplateTemplateParmDecl>(d)) {
                  // TODO: is*
                  tag = "template-parameter";
                  classes.insert("type");
                  classes.insert("template");

                  llvm_unreachable("TemplateTemplateParm declaration not implemented!");

                } else {
                  llvm_unreachable("Unknown Template declaration!");
                }

              } else if (auto const decl = clang::dyn_cast<clang::TypeDecl>(d)) {
                if (auto const decl = clang::dyn_cast<clang::TagDecl>(d)) {
                  if (decl->isStruct())
                    tag = "struct";

                  if (decl->isInterface())
                    tag = "interface";

                  if (decl->isClass())
                    tag = "class";

                  if (decl->isUnion())
                    tag = "union";

                  if (decl->isEnum())
                    tag = "enumeration";

                  if (auto const decl = clang::dyn_cast<clang::EnumDecl>(d)) {
                    tag = "enumeration";

                    if (decl->isScoped())
                      classes.insert("scoped");

                    if (decl->isComplete())
                      attribs.insert("complete");

                  } else if (auto const decl = clang::dyn_cast<clang::RecordDecl>(d)) {
                    if (decl->isAnonymousStructOrUnion())
                      return;

                    if (auto const decl = clang::dyn_cast<clang::CXXRecordDecl>(d)) {
                      if (decl->isLambda()) {
                        tag = "lambda";

                        if (decl->isGenericLambda())
                          classes.insert("template");

                        if (decl->isDependentLambda())
                          classes.insert("dependent");

                      } else if (decl->hasDefinition()) {
                        if (decl->isAggregate())
                          classes.insert("aggregate");

                        if (decl->isPOD())
                          classes.insert("plain");

                        if (decl->isStandardLayout())
                          classes.insert("standard");

                        if (decl->isTrivial())
                          classes.insert("trivial");

                        if (decl->isEmpty())
                          classes.insert("empty");

                        if (decl->isPolymorphic())
                          classes.insert("polymorphic");

                        if (decl->isAbstract())
                          classes.insert("abstract");

                        if (decl->isLiteral())
                          classes.insert("literal");
                      }

                      if (auto const decl = clang::dyn_cast<clang::ClassTemplateSpecializationDecl>(d)) {
                        classes.insert("template");
                        classes.insert("specialization");

                        if (decl->isExplicitInstantiationOrSpecialization())
                          classes.insert("explicit");

                        if (auto const decl = clang::dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(d)) {
                          classes.insert("partial");

                          // if (decl->isMemberSpecialization())
                          // classes.insert("member");

                        } else {
                          // llvm_unreachable("ClassTemplateSpecialization declaration not implemented!");
                        }

                      } else {
                        // llvm_unreachable("CXXRecord declaration not implemented!");
                      }

                    } else {
                      // llvm_unreachable("Record declaration not implemented!");
                    }

                  } else {
                    llvm_unreachable("Tag declaration not implemented!");
                  }

                } else if (auto const decl = clang::dyn_cast<clang::TemplateTypeParmDecl>(d)) {
                  tag = "template-parameter";

                  classes.insert("type");
                  if (decl->isParameterPack())
                    classes.insert("pack");

                } else if (auto const decl = clang::dyn_cast<clang::TypedefNameDecl>(d)) {
                  if (auto const decl = clang::dyn_cast<clang::TypeAliasDecl>(d))
                    tag = "alias";
                  else if (auto const decl = clang::dyn_cast<clang::TypedefDecl>(d))
                    tag = "typedef";
                  else
                    llvm_unreachable("TypedefName declaration not implemented!");

                } else if (auto const decl = clang::dyn_cast<clang::UnresolvedUsingTypenameDecl>(d)) {
                  tag = "typename";
                  classes.insert("using");
                  classes.insert("unresolved");

                } else {
                  // llvm_unreachable("Type declaration not implemented!");
                }

              } else if (auto const decl = clang::dyn_cast<clang::UsingDecl>(d)) {
                // TODO: is*
                tag = "using";
                llvm_unreachable("Using declaration not implemented!");

              } else if (auto const decl = clang::dyn_cast<clang::UsingDirectiveDecl>(d)) {
                // TODO: is*
                tag = "using-directive";
                llvm_unreachable("UsingDirective declaration not implemented!");

              } else if (auto const decl = clang::dyn_cast<clang::UsingShadowDecl>(d)) {
                // TODO: is*
                tag = "using-shadow";
                llvm_unreachable("UsingShadow declaration not implemented!");

              } else if (auto const decl = clang::dyn_cast<clang::ValueDecl>(d)) {
                tag = "value";

                auto const type = decl->getType();
                if (!type.isNull()) {
                  if (type.isLocalConstQualified())
                    attribs.insert("const");

                  if (type.isLocalRestrictQualified())
                    attribs.insert("restrict");

                  if (type.isLocalVolatileQualified())
                    attribs.insert("volatile");

                  attribs.insert("type=\"" + type.getAsString() + "\"");
                }

                if (decl->isWeak())
                  classes.insert("weak");

                if (auto const decl = clang::dyn_cast<clang::DeclaratorDecl>(d)) {
                  if (auto const decl = clang::dyn_cast<clang::FieldDecl>(d)) {
                    // TODO: is*
                    tag = "field";
                    llvm_unreachable("Field declaration not implemented!");

                  } else if (auto const decl = clang::dyn_cast<clang::FunctionDecl>(d)) {
                    // TODO: is*
                    tag = "function";

                    if (decl->isVariadic())
                      classes.insert("variadic");

                    if (decl->isPure())
                      attribs.insert("pure");

                    if (decl->isTrivial())
                      attribs.insert("trivial");

                    if (decl->isVirtualAsWritten())
                      attribs.insert("virtual");

                    if (decl->isConstexpr())
                      attribs.insert("constexpr");

                    if (decl->isMain())
                      classes.insert("main");

                    if (decl->isGlobal())
                      attribs.insert("scope=\"global\"");

                    if (decl->isInlineSpecified())
                      attribs.insert("inline");

                    if (decl->isOverloadedOperator())
                      classes.insert("operator");

                    if (decl->isFunctionTemplateSpecialization()) {
                      classes.insert("template");
                      classes.insert("specialization");
                    }

                    if (auto const decl = clang::dyn_cast<clang::CXXMethodDecl>(d)) {
                      tag = "method";

                      if (!decl->isUserProvided())
                        return;

                      if (decl->isStatic())
                        attribs.insert("static");

                      if (decl->isConst())
                        attribs.insert("const");

                      if (decl->isVolatile())
                        attribs.insert("volatile");

                      if (decl->isVirtual())
                        attribs.insert("virtual");

                      if (decl->isUsualDeallocationFunction())
                        classes.insert("delete");

                      if (decl->isCopyAssignmentOperator())
                        classes.insert("copy");

                      if (decl->isMoveAssignmentOperator())
                        classes.insert("move");

                      if (auto const decl = clang::dyn_cast<clang::CXXConstructorDecl>(d)) {
                        tag = "constructor";

                        if (decl->isExplicit())
                          attribs.insert("explicit");

                        if (decl->isDefaultConstructor())
                          classes.insert("default");

                        if (decl->isCopyConstructor())
                          classes.insert("copy");

                        if (decl->isMoveConstructor())
                          classes.insert("move");

                        llvm_unreachable("CXXConstructor declaration not implemented!");

                      } else if (auto const decl = clang::dyn_cast<clang::CXXConversionDecl>(d)) {
                        tag = "conversion";

                        if (decl->isExplicit())
                          attribs.insert("explicit");

                      } else if (auto const decl = clang::dyn_cast<clang::CXXDestructorDecl>(d)) {
                        tag = "destructor";

                      } else {
                        // llvm_unreachable("CXXMethod declaration not implemented!");
                      }

                    } else {
                      // llvm_unreachable("Function declaration not implemented!");
                    }

                  } else if (auto const decl = clang::dyn_cast<clang::NonTypeTemplateParmDecl>(d)) {
                    tag = "template-parameter";

                    if (decl->isParameterPack())
                      classes.insert("pack");

                    if (decl->isPackExpansion())
                      classes.insert("expansion");

                  } else if (auto const decl = clang::dyn_cast<clang::VarDecl>(d)) {
                    tag = "variable";

                    if (decl->isStaticLocal()) {
                      attribs.insert("static");
                      attribs.insert("scope=\"block\"");
                    }

                    if (decl->isLocalVarDecl())
                      attribs.insert("scope=\"block\"");

                    if (decl->isFunctionOrMethodVarDecl())
                      attribs.insert("scope=\"function\"");

                    if (decl->isStaticDataMember())
                      attribs.insert("static");

                    if (decl->isFileVarDecl())
                      attribs.insert("scope=\"global\"");

                    if (decl->isExceptionVariable())
                      classes.insert("exception");

                    if (decl->isNRVOVariable())
                      classes.insert("return");

                    if (decl->isCXXForRangeDecl())
                      classes.insert("foreach");

                    if (decl->isConstexpr())
                      attribs.insert("constexpr");

                    if (auto const decl = clang::dyn_cast<clang::ImplicitParamDecl>(d)) {
                      tag = "parameter";
                      classes.insert("implicit");

                      llvm_unreachable("ImplicitParam declaration not implemented!");

                    } else if (auto const decl = clang::dyn_cast<clang::ParmVarDecl>(d)) {
                      tag = "parameter";

                      if (decl->isParameterPack())
                        classes.insert("pack");

                    } else if (auto const decl = clang::dyn_cast<clang::VarTemplateSpecializationDecl>(d)) {
                      classes.insert("template");
                      classes.insert("specialization");

                      if (auto const decl = clang::dyn_cast<clang::VarTemplatePartialSpecializationDecl>(d)) {
                        classes.insert("partial");
                      } else {
                        // llvm_unreachable("VarTemplateSpecialization declaration not implemented!");
                      }

                    } else {
                      // llvm_unreachable("Var declaration not implemented!");

                    }
                  } else {
                    // llvm_unreachable("Declarator declaration not implemented!");

                  }
                } else if (auto const decl = clang::dyn_cast<clang::EnumConstantDecl>(d)) {
                  tag = "constant";
                  classes.insert("enumeration");

                } else if (auto const decl = clang::dyn_cast<clang::IndirectFieldDecl>(d)) {
                  tag = "indirect-field";
                  llvm_unreachable("IndirectField declaration not implemented!");

                } else if (auto const decl = clang::dyn_cast<clang::UnresolvedUsingValueDecl>(d)) {
                  tag = "value";
                  classes.insert("using");
                  classes.insert("unresolved");

                  llvm_unreachable("UnresolvedUsingValue declaration not implemented!");

                } else {
                  llvm_unreachable("Value declaration not implemented!");

                }
              } else {
                llvm_unreachable("Unknown named declaration type!");

              }
            } else {
              llvm_unreachable("Unknown declaration type!");

            }
          } else {
            llvm_unreachable("Unknown type!");
          }
        }

        std::stringstream stream;
        stream << tag << "#" << name << "[";
        for (auto const& attrib : attribs) {
          stream << (&attrib == &*attribs.begin() ? "" : ",") << attrib;
        }
        stream << "]";
        for (auto const classe : classes) {
          stream << ":" << classe;
        }
        llvm::errs() << stream.str() << "\n";

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

          if (decl->isWeak())
            ;
          else
            ;

        } else if (auto decl = result.Nodes.getNodeAs<clang::ValueDecl>("value")) {
          llvm_unreachable("Unknown value declaration!");
        } else if (auto decl = result.Nodes.getNodeAs<clang::NamedDecl>("named")) {
          llvm_unreachable("Unknown named declaration type!");
        }

        if (auto decl = result.Nodes.getNodeAs<clang::NamespaceDecl>("decl")) {
          if (!source_manager.isWrittenInMainFile(decl->getLocation()))
            return;

          if (decl->isAnonymousNamespace())
            return;

          auto style = swang::config::style();
          if (decl->isInline() && config.inline_namespace_style.is_set)
            style = config.inline_namespace_style;
          else if (config.namespace_style.is_set)
            style = config.namespace_style;

          if (!style.is_set)
            return;

          auto const diagnostic = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "inline namespace '%0' hasn't got swag like '%1%2%3'.");
          auto       name       = std::string();
          auto       matches = true;
          std::tie(matches, name) = check(decl->getName().str(), style);

          auto const source     = clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(decl->getSourceRange()), source_manager, clang::LangOptions());
          auto const nameOffset = source.find(name, source.find("namespace") + std::strlen("namespace"));
          auto const nameStart  = decl->getLocStart().getLocWithOffset(nameOffset);
          auto const nameEnd    = decl->getLocStart().getLocWithOffset(nameOffset + name.size() - 1);
          auto const nameRange  = clang::CharSourceRange::getTokenRange(nameStart, nameEnd);

          report(nameRange, diagnostic, name, style);

        } else if (auto decl = result.Nodes.getNodeAs<clang::RecordDecl>("decl")) {
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
      bool                          config_initialized;
  };

} // end anonymous namespace

auto build_path = llvm::cl::opt<std::string>(llvm::cl::Positional, llvm::cl::desc("<build-path>"));
auto source_paths = llvm::cl::list<std::string>(llvm::cl::Positional, llvm::cl::desc("<source0> [... <sourceN>]"), llvm::cl::OneOrMore);

int main(int argc, const char** argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  auto database = std::unique_ptr<clang::tooling::CompilationDatabase>(clang::tooling::FixedCompilationDatabase::loadFromCommandLine(argc, argv));
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
  auto finder = clang::ast_matchers::MatchFinder();
  auto callback = SwangCallback(&tool.getReplacements());

  auto declMatcher = clang::ast_matchers::decl().bind("decl");

  finder.addMatcher(clang::ast_matchers::decl(declMatcher), &callback);
  // finder.addMatcher(clang::ast_matchers::declRefExpr(clang::ast_matchers::hasDeclaration(declMatcher)), &callback);

  return tool.run(clang::tooling::newFrontendActionFactory(&finder).get());
}
