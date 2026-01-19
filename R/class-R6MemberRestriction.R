#' @include internals-.capture.R internals-.indent.R
# ================================ > Title < ================================= #
#' @title
#' `r .doc_topic_set("R6MemberRestriction")`
#' `r .doc_topic()`:
#' Validity constraints for `r .doc_r6member(link="html")` objects
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > Description < ============================== #
#' @description
#' `r .doc_tag("sec-description")`
#' A `r .doc_ref_self()` encapsulates and manages a single validation rule -- a `r .doc_ref(tag = "restriction")` -- as a reusable object.
#' This validation rule is a user-supplied `r .doc_type_function()` to check whether `r .doc_phrase_value()`s meet a constraint.
#'
#' A `r .doc_ref_self()` handles the execution of this rule, manages `r .doc_error()` reporting, and provides a consistent interface.
#' `r .doc_ref_self()` objects are primarily used to validate the
#' `r .doc_ref(topic="R6Member",tag="value",text="value")`
#' attribute of
#' `r .doc_r6member()` objects,
#' but can be applied to any `r .doc_phrase_value()` for which a validation rule needs to be enforced.
#'
#' Each `r .doc_ref_self()` has a unique `r .doc_ref(tag="id")` for error reporting and a `r .doc_ref(tag="restriction")` function that implements the actual validation logic.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# =============================== > Details < ================================ #

#' @section `r .doc_section("Details")`
#'
#' A `r .doc_ref_self()` is characterized
#' by its `r .doc_ref(tag="restriction")`, a
#' `r .doc_type_function()`
#' specifying a validation rule for an object.
#' While the validation logic is entirely defined by the `r .doc_r6memberrestriction_restriction()`,
#' the `r .doc_ref_self()` class provides the infrastructure for how the rule is invoked and how results are reported.
#' This separation promotes modularity and consistency across validation workflows.
#'
#' ## Key Features
#' - **Encapsulation**: Wraps validation logic in a structured object with metadata
#' - **Error Handling**: Automatically captures and formats error messages from the restriction function
#' - **Reusability**: Can be combined with other `r .doc_ref_self(suffix="s")` in a `r .doc_r6memberlist()`
#' - **Identification**: Each restriction has a unique identifier for clear error reporting
#' - **Flexibility**: Supports both simple boolean returns and detailed error throwing
# ======================== > Restrictions: Examples < ======================== #
#' ## `r .doc_tag("sec-details-restrictions-examples")` Typical Examples for `r .doc_ref(tag="restriction",text="Restrictions")`
#'
#' - `r .doc_phrase_value()` must be of a certain type (such as `r .doc_fmt_cslist(.doc_type_numeric(),.doc_type_list(),.doc_type_function(),last=" or ")`)
#' - `r .doc_phrase_value()`'s elements *together* must meet a *common* restriction, e.g. in terms of dimensionality `r .doc_phrase_restriction_example(attr = "length", link = "base:length",rhs=" == k")` or totals `r .doc_phrase_restriction_example(attr = "sum", link = "base:sum",rhs=" > t")`
#' - `r .doc_phrase_value()`'s elements *each* must meet an *individual* restriction, e.g. be non-negative `r .doc_phrase_restriction_example(attr = "all", link = "base:all",value="value >= 0")`
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ======================= > Restrictions: Structure < ======================== #
#' ## `r .doc_tag("sec-details-restrictions-structure")` Structure of `r .doc_ref(tag="restriction",text="Restrictions")`
#' The `r .doc_r6memberrestriction_restriction()` must be a `r .doc_type_function()` taking a single argument and returning a single
#' `r .doc_type_logical()` value to indicate
#' if a restriction is met (`TRUE`) or not (`FALSE`).
#' Alternatively, if validation fails, the `r .doc_type_function()` may throw an `r .doc_error()` providing detailed
#' information on how or why the `r .doc_ref(tag="restriction")` is violated.
#' The is `r .doc_r6memberrestriction_restriction()` must be able to take arguments of all kinds and shapes.
#' It is tested using a range of sample inputs as its argument;
#' any violation raises a descriptive `r .doc_error()` so users know how to fix their rule.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================= > Validation Workflow < ========================== #
#' ## Validation Workflow
#' The
#' `r .doc_ref("restriction")`
#' is executed via the
#' `r .doc_ref(tag="check", prefix=paste0(.doc_ref_self(code=FALSE),"$"),suffix="()")`
#' method,
#' which applies the rule to a provided value and returns a `r .doc_type_list()` containing both the validation decision
#' and any error message encountered.
#' This encapsulation allows validation results to be easily captured, logged, or aggregated.
#'
#' The validation workflow includes:
#' 1. Call the `r .doc_ref(tag="check")` method with a value to validate
#' 2. The restriction function is executed with the provided value
#' 3. The return value (logical or error) is captured and formatted
#' 4. A list with `decision` (TRUE/FALSE) and `message` (empty or error text) is returned
#' 5. Callers can use this structure to implement custom validation strategies
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > Composition of Rules < ============================== #
#' ## Composition of Rules
#' Multiple `r .doc_ref_self()` objects are commonly combined to define complex validation logic,
#' typically for specifying
#' `r .doc_r6member()` objects.
#' This compositional design keeps individual rules small, testable, and focused on a single concern,
#' promoting code clarity and maintainability.
#'
#' See the `r .doc_ref_section("examples",topic="R6Member",text="R6Member examples")` for practical examples.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > Examples < ============================== #
#' @section `r .doc_section("Examples")`
#' ```{r, eval=TRUE,echo=FALSE,include=FALSE}
#'  knitr::opts_chunk$set(eval=TRUE)
#' ```
#' ```{r child = "man-roxygen/examples/class-R6MemberRestriction-initialize.Rmd"}
#' ```
#' ```{r child = "man-roxygen/examples/class-R6MemberRestriction-check.Rmd"}
#' ```
# ────────────────────────────────── <end> ─────────────────────────────────── #
#' @export
R6MemberRestriction <-
    R6::R6Class(
        "R6MemberRestriction",
        # =============================== > private < ================================ #

        private = list(
            # ============================== > attributes < ============================== #
            .id = "",
            .restriction = NULL,
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================== > .accessor < =============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ get or set a private field:                                            ││ #
            # ││      Dispatches to `.<field>` if called without, and                   ││ #
            # ││      `.set_<field>` if called with argument                            ││ #
            # ││ Returns                                                                ││ #
            # ││      The value of `.<field>` if called without argument                ││ #
            # ││        (dispatches to `.<field>`),                                     ││ #
            # ││      or `NULL` if called with argument                                 ││ #
            # ││        (dispatches to `.set_<field>`)                                  ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .accessor = function(new_val, field_name) {
                if (!exists(paste0(".", field_name), private)) {
                    stop(paste0("Invalid field_name '", field_name, "'!"))
                }
                if (missing(new_val)) {
                    return(private[[paste0(".", field_name)]])
                } else {
                    setter_name <-
                        paste0(".set_", field_name)

                    if (!exists(setter_name, envir = private)) {
                        stop(paste0("private$", setter_name, " not found!"))
                    }
                    return(private[[setter_name]](new_val))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > .set_* < ================================ #

            # =============================== > .set_id < ================================ #
            .set_id = function(new_id) {
                # Make sure id is not set to `NULL`
                if (is.null(new_id)) {
                    stop(paste0(private$.id, ": id must not be `NULL`!"))
                }
                # Make sure id is single element
                if (length(new_id) != 1) {
                    stop(paste0(private$.id, ": id must be length 1!"))
                }
                # Make sure it is not set to only whitespaces
                if (grepl("^\\s*$", new_id)) {
                    stop(paste0(private$.id, ": id must not be empty string!"))
                }
                private$.id <-
                    new_id

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =========================== > .set_restriction < =========================== #

            .set_restriction = function(new_restriction) {
                # Make sure restriction is a restriction
                if (!is.function(new_restriction)) {
                    stop(paste0(
                        private$.id,
                        ": restriction must be a function"
                    ))
                }

                catch <-
                    lapply(
                        list(
                            1,
                            1:5,
                            runif(5),
                            letters[1],
                            letters[1:5],
                            as.list(1),
                            as.list(1:5),
                            as.list(letters[1]),
                            as.list(letters[1:5]),
                            paste0(letters[sample.int(26, 8)], collapse = "")
                        ),
                        function(x) {
                            private$.check_restriction_validity(
                                new_restriction,
                                x
                            )
                        }
                    )
                private$.restriction <-
                    new_restriction

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =============================== > .check_* < =============================== #
            # ========================== > .check_restriction < ========================== #

            .check_restriction = function(
                value,
                include_id = TRUE,
                include_value = TRUE
            ) {
                indent_level <-
                    3
                output <-
                    tryCatch(
                        list(
                            decision = private$.restriction(value),
                            message = ""
                        ),
                        error = function(e) {
                            list(
                                decision = FALSE,
                                message = e$message
                            )
                        }
                    )
                if (is.null(output$decision)) {
                    output$decision <-
                        TRUE
                }
                if (!output$decision) {
                    if (output$message == "") {
                        output$message <-
                            paste0(
                                "`",
                                .capture(
                                    private$.restriction,
                                    collapse = "\n"
                                ),
                                "`"
                            )
                    }

                    if (include_value) {
                        output$message <-
                            paste0(
                                .indent(
                                    text = output$message,
                                    times = indent_level
                                ),
                                "\n",
                                "not met by value",
                                "\n",
                                .indent(
                                    paste0(
                                        "`",
                                        .capture(value),
                                        "`"
                                    ),
                                    times = indent_level
                                )
                            )
                    }

                    if (include_id) {
                        output$message <-
                            paste0(
                                private$.id,
                                ":\n",
                                .indent(output$message, times = indent_level)
                            )
                    }
                }
                return(output)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ===================== > .check_restriction_validity < ====================== #

            .check_restriction_validity = function(restriction, val) {
                is_error <-
                    FALSE
                return_val <-
                    tryCatch(restriction(val), error = function(e) {
                        assign(
                            "is_error",
                            TRUE,
                            pos = parent.env(environment())
                        )
                        TRUE
                    })
                if (is.null(return_val)) {
                    return_val <-
                        TRUE
                }

                throw_error <-
                    length(return_val) != 1 ||
                    !is.logical(return_val) ||
                    (!return_val %in% c(FALSE, TRUE) &&
                        !is_error)

                if (throw_error) {
                    stop(paste0(
                        "\r",
                        private$.id,
                        ":\nfunction must return a single logical value or throw an error, but",
                        "\n",
                        "\t`",
                        gsub(
                            "(\n)",
                            "\\1\t",
                            .capture(restriction)
                        ),
                        "`",
                        "\n",
                        "failed to do so for input",
                        "\n",
                        "\t`",
                        gsub(
                            "(\n)",
                            "\\1\t",
                            .cut_string(
                                .capture(val),
                                total_length = .warn_length_minus(0) / 3
                            )
                        ),
                        "`",
                        "\n",
                        "for which it returned",
                        "\n\t",
                        "`",
                        .cut_string(
                            .capture(restriction(val)),
                            total_length = .warn_length_minus(0) / 3
                        ),
                        "`"
                    ))
                }
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        ),

        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # ================================ > public < ================================ #

        public = list(
            # =============================== > methods < ================================ #
            # ================================ > check < ================================= #
            #' @details
            #' Check whether a given `r .doc_phrase_value()` satisfies the `r .doc_ref(tag="restriction")`.
            #' `r .doc_fmt_highlight(c("This is the core method of", .doc_ref_self(), "objects."))`
            #'
            #' This method executes the restriction function against the provided value and returns
            #' a list containing both the validation decision (`TRUE` or `FALSE`) and any error
            #' messages that may have been generated. The method automatically formats error information
            #' to include the restriction identifier and the problematic value (if requested).
            #'
            #' @param value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value"}
            #' ```
            #'
            #' @param include_id
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="include_id"}
            #' ```
            #'
            #' @param include_value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="include_value"}
            #' ```
            #'
            #' @return
            #' `r .doc_type_list()`
            #' with entries
            #'
            #' ```{r,eval=TRUE,echo=FALSE,results="asis"}
            #' .doc_tbl(
            #'   table = rbind(
            #'     c(
            #'       "decision",
            #'       .doc_type_logical(brackets = TRUE),
            #'       paste(
            #'         "indicator whether ",
            #'         "the", .doc_phrase_value(),
            #'         "meets (`TRUE`)",
            #'         "or violates (`FALSE`) the",
            #'         paste0(.doc_ref(tag="restriction"),".")
            #'       )
            #'     ),
            #'     c(
            #'       "message",
            #'       .doc_type_character(brackets = TRUE),
            #'       paste(
            #'         "error message resulting from",
            #'         .doc_r6memberrestriction_restriction(),
            #'         "(if any),",
            #'         "or empty string otherwise"
            #'       )
            #'     )
            #'   )
            #' )
            #' ```
            #'
            check = function(value, include_id = TRUE, include_value = TRUE) {
                private$.check_restriction(
                    value = value,
                    include_id = include_id,
                    include_value = include_value
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================== > initialize < ============================== #

            #' @details
            #' `r .doc_phrase_constructor()`
            #' with
            #' identifier `r .doc_ref("id")`
            #' and
            #' restriction `r .doc_ref("restriction")`
            #'
            #' Creates a new `r .doc_ref_self()` object with the specified validation function
            #' and a unique identifier. The restriction function is validated during initialization
            #' to ensure it properly returns logical values and can handle various input types.
            #'
            #' @param id `r .doc_tag(tag = "sec-params")`
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="id"}
            #' ```
            #'
            #' @param restriction
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restriction"}
            #' ```
            #'
            #' @return
            #' `r .doc_ref_self()` object.
            #' The newly created object has the `r .doc_ref(tag="id")` and `r .doc_ref(tag="restriction")`
            #' fields initialized and ready for use with the `r .doc_ref(tag="check")` method.
            #'
            initialize = function(
                id = format(Sys.time(), "%Y-%M-%d - %H:%M:%S"),
                restriction = function(x) TRUE
            ) {
                self$id <-
                    id
                self$restriction <-
                    restriction
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > print < ================================= #

            #' @details
            #' Print method for `r .doc_ref_self()` objects that
            #' displays information about the restriction (`r .doc_ref(tag="id")` and
            #' `r .doc_ref(tag="restriction")`) in a human-readable format.
            #'
            #' This method is useful for debugging and understanding the structure of
            #' validation rules, especially when working with multiple restrictions
            #' combined in a `r .doc_r6memberlist()`.
            #'
            #' @param restriction_level
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restriction_level"}
            #' ```
            #'
            #' @return
            #' `r .doc_value(NULL)` (invisibly). The method is called for its side effect of printing.
            #'
            #' @export

            print = function(restriction_level = 2) {
                if (restriction_level > 0) {
                    msg <-
                        self$id
                    if (restriction_level > 1) {
                        msg <-
                            paste0(
                                msg,
                                ":\n",
                                .indent(
                                    paste0(
                                        "`",
                                        gsub(
                                            "(function\\s*\\(.*?\\))\\s*\\{",
                                            "\\1 {",
                                            .capture(
                                                self$restriction,
                                                collapse = "\n"
                                            ),
                                            perl = TRUE
                                        ),
                                        "`"
                                    ),
                                    2
                                )
                            )
                    }
                    cat(msg)
                }
                return(invisible(NULL))
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ────────────────────────────────── <end> ─────────────────────────────────── #
        ),
        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # =========================== > active bindings < ============================ #
        active = list(
            # ================================== > id < ================================== #

            #' @field id
            #' ```{r, child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="id"}
            #' ```
            id = function(new_id) {
                private$.accessor(new_id, "id")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > restriction < ============================== #
            #' @field restriction
            #' ```{r, child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restriction"}
            #' ```
            #'
            restriction = function(new_restriction) {
                private$.accessor(new_restriction, "restriction")
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        )
        # ────────────────────────────────── <end> ─────────────────────────────────── #
    )
