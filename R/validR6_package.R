#' `r .doc_topic_set("validR6-package",check_doc_function=FALSE)`
#' @include internals-.capture.R
#'
#' @title The `validR6` package
#' @docType package
#'
#' @description
#' `r .doc_ref_link(topic="R6MemberRestriction", tag="check", text="CHECK", link = TRUE)`
#' `r .doc_ref_link(topic="R6MemberRestriction", text="No Tag", link = TRUE)`
#' The `r .doc_ref_self(code=FALSE)`
#'      provides a validation framework for members (fields, methods and active bindings) in
#'      `r .doc_r6class()`.
#' This ensures that members meet validation criteria, e.g. in terms of their type, structure or content.
#' Validation happens *before* assignment and is reusable across `r .doc_r6class()`.
#'
#' @details
#' To declare validation rules for
#' `r .doc_r6class()` members',
#' this package revolves around the three cooperating `r .doc_r6class(suffix="es")`:
#' `r .doc_fmt_cslist(.doc_r6memberrestriction(), .doc_r6member(),.doc_r6memberlist())`.
#' These are outlined in the following.
#'
#' ## `r .doc_section(.doc_r6memberrestriction(code=FALSE,link=FALSE),level=2)`
#' `r .doc_r6memberrestriction()` objects
#'      are wrappers for user-defined validation rules,
#'      standardizing how rules are executed and errors are captured.
#'
#' ## `r .doc_section(.doc_r6member(code=FALSE,link=FALSE),level=2)`
#' `r .doc_r6member()` objects
#'      encapsulate exactly
#'      one member (field, method or active binding) of `r .doc_r6class()`
#'      and store the member's current value.
#' They may contain an optional set of `r .doc_r6memberrestriction(suffix="s")`, which are used for validating new values before assignment via the `r .doc_ref_method(topic="R6MemberRestriction",tag="check")` method.
#' The use of a `r .doc_r6member()` object as field, method, or active binding inside `r .doc_r6class()`
#'      is simplified through its
#'      `r .doc_ref_section(topic="R6Member",section="methods",text="helper methods")`.
#'
#' ## `r .doc_section(.doc_r6memberlist(code=FALSE,link=FALSE),level=2)`
#' `r .doc_r6memberlist()` objects
#'      are lightweight wrappers around any number of `r .doc_r6member()` instances.
#' They provide `r .doc_ref_section(topic="R6MemberList",section="methods",text="helper methods")`
#'      to build fully validated `r .doc_r6class()` by
#'      generating the corresponding
#'      `r .doc_fmt_cslist(.doc_ref(tag="public",topic="R6Class",namespace="R6",code=!FALSE), .doc_ref(tag="private",topic="R6Class",namespace="R6",code=!FALSE), .doc_ref(tag="active",topic="R6Class",namespace="R6",suffix="",code=!FALSE))`
#'      `r .doc_type_list(suffix="s")`
#'      used as arguments in
#'      `r .doc_r6class(prefix="R6::", suffix='()')`.
#'
#' @section `r .doc_section("Typical workflow")`
#' 1. Define one or more `r .doc_r6memberrestriction()` objects with validation rules.
#' 2. Define one `r .doc_r6member()` object for each member in a `r .doc_r6class(suffix="")` to be defined and specify the `r .doc_r6memberrestriction(suffix="s")` this member must satisfy.
#' 3. Combine the `r .doc_r6member(suffix="s")` in a `r .doc_r6memberlist()` and call `r .doc_ref_method(tag="R6Class",topic="R6MemberList")` to build a fully validated `r .doc_r6class()`.
#'
#' This keeps validation logic decoupled, reusable, and testable, while letting `r .doc_r6class(text="R6",suffix="")` remain concise.
#'
#' @section `r .doc_section("Key benefits")`
#' * **Consistency**: uniform validation and error reporting across members and classes.
#' - **Reusability**: compose simple rules into complex constraints.
#' - **Safety**: validation occurs before assignment, preventing invalid state.
#' - **Convenience**: helpers generate `r .doc_r6class()`, minimizing boilerplate.
"_PACKAGE"
