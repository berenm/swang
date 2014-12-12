/**
 * @file
 *
 * Distributed under the Boost Software License, Version 1.0.
 * See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt
 */

#ifndef __INCLUDED_SWANG_CONFIG_HPP__
#define __INCLUDED_SWANG_CONFIG_HPP__

#include <string>

#include "llvm/Support/Regex.h"
#include "llvm/ADT/StringRef.h"

namespace swang {

  enum class language_type {
    none,
    cpp,
  };

  namespace casing {

    enum type {
      any_case=0,
      lower_case,
      camel_back,
      upper_case,
      camel_case,
    };

  }

  struct config {
    config() :
      language(language_type::none) {}

    language_type language;

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
    style record_template_style;
    style specialization_style;
    style declarator_style;
    style function_template_style;
    style friend_style;
    style using_style;
    style unresolved_style;
    style named_style;
    style decl_style;

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
    style exception_variable_style;
    style constant_style;
    style variable_style;

    style parameter_constant_style;
    style parameter_pack_style;
    style parameter_style;

    style lambda_style;
    style interface_style;
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
  };

  config get_config(std::string config_name, std::string file_name, std::string fallback_name);

} // namespace swang

#endif // ifndef __INCLUDED_SWANG_CONFIG_HPP__
