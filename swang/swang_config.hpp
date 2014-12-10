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

  enum casing_type {
    any_case=0,
    lower_case,
    upper_case,
    camel_case,
    camel_back,
  };

  struct config {
    language_type language = language_type::none;

    struct style {
      bool        is_set = false;
      casing_type casing = casing_type::any_case;
      std::string prefix = "";
      std::string suffix = "";
    };

    style namespace_style;
    style inline_namespace_style;
    style record_style;
    style record_template_style;
    style specialization_style;
    style declarator_style;
    style parameter_style;
    style access_style;
    style constructor_style;
    style destructor_style;
    style enum_style;
    style constant_style;
    style method_style;
    style variable_style;
    style field_style;
    style function_style;
    style function_template_style;
    style friend_style;
    style using_style;
    style unresolved_style;
    style named_style;
    style decl_style;
  };

  swang::config get_config(std::string config_name, std::string file_name, std::string fallback_name);

}

#endif // ifndef __INCLUDED_SWANG_CONFIG_HPP__
