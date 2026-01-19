#' @include internals-.capture.R
#'
#' @aliases R6Member
#'
# ================================ > title < ================================= #
#' @title
#' `r .doc_topic_set("R6Member")`
#' Container for members
#' in `r .doc_r6class(suffix="")` definitions.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > Description < ============================== #
#' @description
#' `r .doc_tag("sec-description")`
#' `r .doc_ref(topic="R6MemberRestriction",tag="check")`
#' A container that stores name, value and (optional) validity restrictions of type `r .doc_r6memberrestriction()`
#' for members to be used in `r .doc_r6class(suffix="")` declarations.
#'
#'  A `r .doc_ref_self()` object provides
#' * a setter that checks restrictions
#' * a getter returning the value
#' * helper methods for using it as member (attribute or method) in `r .doc_r6class()`
#'
#' See the sections on
#' `r .doc_ref_section("details")`,
#' `r .doc_ref_section("examples")`
#' and
#' `r .doc_ref_section("methods")`.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# =============================== > Details < ================================ #
#' @section `r .doc_section("Details")`
#' `r .doc_ref_self()` objects
#' provide a formalized interface for setting up members of
#' `r .doc_r6class()`
#' and provide methods for automatically generating the corresponding
#' `r .doc_r6class(suffix="()")` arguments
#' `r .doc_r6class_public()`, `r .doc_r6class_private()` and `r .doc_r6class_active()`.
#'
#' ## Validation rules for `r .doc_ref_field(tag="value")` `r .doc_tag("sec-details-restricted")`
#' Validation rules for the
#'      `r .doc_ref_field(tag="value")`
#'      field
#'      are specified in the
#'      `r .doc_ref_field(tag="restrictions")` field.
#'
#' `r .doc_ref_field(tag="restrictions")` contains a list of zero or more
#'      `r .doc_r6memberrestriction(suffix="s")`
#'      describing the validation rules for the `r .doc_ref_field(tag="value")` field.
#' Any new `r .doc_ref_field(tag="value",text="value")` is passed to the
#'      `r .doc_ref_method(topic="R6MemberRestriction",tag="check")` method of all elements in `r .doc_ref_field(tag="restrictions")`
#'      before `r .doc_ref_field(tag="value",text=paste0(.doc_topic(),"$value"))` is changed.
#' This ensures all validation criteria are met.
#'
#' ## Combining multiple `r .doc_ref_self(suffix="s")` and declaring `r .doc_r6class(suffix="es")` `r .doc_tag("sec-details-multiple")`
#' For declaring `r .doc_r6class(suffix="es")`,
#' `r .doc_ref_self(suffix="s")` are most conveniently combined in a `r .doc_r6memberlist()`.
#'
#'
# ────────────────────────────────── <end> ─────────────────────────────────── #
#'
# =============================== > Examples < =============================== #
#' @section `r .doc_section("Examples")`
#' ```{r, child = "man-roxygen/examples/class-R6MemberRestriction-initialize.Rmd", opts.label="examples"}
#' ```
#' ```{r, child = "man-roxygen/examples/class-R6Member-initialize-valid.Rmd", opts.label="examples"}
#' ```
#' ```{r, child = "man-roxygen/examples/class-R6Member-initialize-invalid.Rmd", opts.label="examples"}
#' ```
# ────────────────────────────────── <end> ─────────────────────────────────── #
#' @export
R6Member <-
    R6::R6Class(
        "R6Member",
        # ============================ > private < ============================= #

        private = list(
            # ============================ > attributes < ============================= #
            .is_method = FALSE,
            .is_private = FALSE,
            .locked = TRUE,
            .name = NULL,
            .private_prefix = ".",
            .private_suffix = "",
            .restrictions = NULL,

            .value = NULL,
            .value_locked = TRUE,

            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =============================== > methods < ================================ #
            # ============================== > .accessor < =============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Universal accessor for getting/setting private fields                  ││ #
            # ││ Acts as both getter and setter.                                        ││ #
            # ││ Enforces locking policies and validates assignments before update.     ││ #
            # ││ Returns:                                                               ││ #
            # ││   - The field's value if called without argument (getter)              ││ #
            # ││   - Invisibly NULL if called with argument (setter)                    ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .accessor = function(new_val, field_name) {
                if (!exists(paste0(".", field_name), private)) {
                    stop(paste0(
                        "Invalid field_name '",
                        .cut_string(
                            field_name,
                            total_length = .warn_length_minus(22)
                        ),
                        "'!"
                    ))
                }
                if (missing(new_val)) {
                    return(private[[paste0(".", field_name)]])
                } else {
                    setter_name <-
                        paste0(".set_", field_name)

                    if (!exists(setter_name, envir = private)) {
                        stop(paste0(
                            private$.err_template(new_val, field_name),
                            "Field is private!"
                        ))
                    }

                    if (
                        private$.locked &&
                            !(field_name %in%
                                c("locked", "value_locked", "value"))
                    ) {
                        stop(paste0(
                            private$.err_template(new_val, field_name),
                            self$name,
                            " is locked!"
                        ))
                    }
                    if (
                        private$.value_locked &&
                            (field_name %in% c("value"))
                    ) {
                        stop(paste0(
                            private$.err_template(new_val, field_name),
                            self$name,
                            "$",
                            field_name,
                            " is locked!"
                        ))
                    }
                    private$.check_null(new_val, field_name)

                    if (!is.null(new_val)) {
                        private$.check_type(new_val, field_name)
                        private$.check_length(new_val, field_name)
                        private$.check_na(new_val, field_name)
                        private$.check_element_type(new_val, field_name)
                    }

                    return(private[[setter_name]](new_val))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ========================= > .ensure_restrictions < ========================= #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Validate value against all restrictions                                ││ #
            # ││ Checks the value against each R6MemberRestriction in the list.        ││ #
            # ││ Throws an error immediately if any restriction fails.                  ││ #
            # ││ Returns `TRUE` if all checks pass or restrictions are empty/NULL.       ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .ensure_restrictions = function(
                value = private$.value,
                restrictions = private$.restrictions,
                err_type = c("value", "restrictions")
            ) {
                if (
                    is.null(value) ||
                        is.null(restrictions) ||
                        (length(restrictions) < 1)
                ) {
                    return(TRUE)
                }

                restriction_checks <-
                    lapply(
                        seq_along(restrictions),
                        function(i) {
                            restrictions[[i]]$check(
                                value,
                                include_id = TRUE,
                                include_value = FALSE
                            )
                        }
                    )

                failed_restriction_checks <-
                    which(
                        !vapply(
                            restriction_checks,
                            function(x) x$decision,
                            TRUE
                        )
                    )

                if (length(failed_restriction_checks) > 0) {
                    private$.err_violated_restrictions(
                        value = value,
                        restrictions = restrictions,
                        restriction_msgs = vapply(
                            restriction_checks[
                                failed_restriction_checks
                            ],
                            function(restriction_check) {
                                restriction_check$message
                            },
                            character(1)
                        ),
                        err_type = err_type
                    )
                }

                return(TRUE)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================ > .pos_in_parent < ============================ #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Generate reference string for accessing member in R6 context           ││ #
            # ││ Constructs appropriate accessor path (self$`name` or                   ││ #
            # ││ private$`.name`). Used internally for generating R6 initialization.    ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .pos_in_parent = function(type = c("public", "private")) {
                type <-
                    match.arg(type)
                if (type == "public") {
                    paste0("self$`", private$.name, "`")
                } else if (type == "private") {
                    paste0("private$`", private$.private_name(), "`")
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================ > .private_name < ============================= #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Compute the private internal name for this member                      ││ #
            # ││ Constructs name as: prefix + name + suffix (e.g., ".name")            ││ #
            # ││ Used to store the member in the private environment.                   ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #
            .private_name = function() {
                paste0(
                    private$.private_prefix,
                    private$.name,
                    private$.private_suffix
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # =============================== > .check_* < =============================== #
            # ============================ > .check_length < ============================= #
            .check_length = function(new_val, field_name) {
                expected_length <-
                    switch(
                        field_name,
                        "name" = 1,
                        "locked" = 1,
                        "value_locked" = 1,
                        "is_method" = 1,
                        "is_private" = 1,
                        NA
                    )
                if (is.na(expected_length)) {
                    return(invisible(NULL))
                }

                if (length(new_val) != expected_length) {
                    stop(paste0(
                        private$.err_template(
                            new_val,
                            field_name
                        ),
                        "Expected length (",
                        expected_length,
                        ")",
                        " != ",
                        "length of assignment",
                        " (",
                        length(new_val),
                        ")"
                    ))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ============================== > .check_na < =============================== #

            .check_na = function(new_val, field_name) {
                allow_na <-
                    !(field_name %in%
                        c(
                            "name",
                            "locked",
                            "value_locked",
                            "is_method",
                            "is_private"
                        ))

                if (allow_na) {
                    return(invisible(NULL))
                }

                if (any(is.na(Reduce(c, unlist(new_val, recursive = TRUE))))) {
                    stop(paste0(
                        private$.err_template(
                            new_val,
                            field_name
                        ),
                        "Assignment must not contain `NA`"
                    ))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > .check_null < ============================== #

            .check_null = function(new_val, field_name) {
                allow_null <-
                    !(field_name %in%
                        c(
                            "name",
                            "locked",
                            "value_locked",
                            "is_method",
                            "is_private"
                        ))

                if (allow_null) {
                    return(invisible(NULL))
                }

                if (
                    any(is.null(Reduce(c, unlist(new_val, recursive = TRUE))))
                ) {
                    stop(paste0(
                        private$.err_template(
                            new_val,
                            field_name
                        ),
                        "Assignment must not be `NULL`"
                    ))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ============================= > .check_type < ============================== #

            .check_type = function(new_val, field_name) {
                expected_type <-
                    switch(
                        field_name,
                        name = c("character"),
                        restrictions = c("list"),
                        locked = c("logical"),
                        value_locked = c("logical"),
                        is_method = c("logical"),
                        is_private = c("logical"),
                        NA
                    )
                if (is.na(expected_type)) {
                    return(invisible(NULL))
                }
                if (!all(expected_type %in% class(new_val))) {
                    stop(paste0(
                        private$.err_template(new_val, field_name),
                        field_name,
                        " must be of class(es)",
                        "\n",
                        .indent(
                            paste0("`", .capture(expected_type), "`"),
                            3
                        ),
                        "\n",
                        "but assignment has class(es)",
                        "\n",
                        .indent(
                            paste0("`", .capture(class(new_val)), "`"),
                            3
                        )
                    ))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ========================= > .check_element_type < ========================== #

            .check_element_type = function(new_val, field_name) {
                expected_type <-
                    switch(
                        field_name,
                        restrictions = c("R6MemberRestriction"),
                        NA
                    )
                if (is.na(expected_type)) {
                    return(invisible(NULL))
                }
                type_checks <-
                    vapply(
                        new_val,
                        function(elem) {
                            !all(expected_type %in% class(elem))
                        },
                        TRUE
                    )

                if (any(type_checks)) {
                    fail_indices <-
                        which(type_checks)

                    fail_classes <-
                        lapply(
                            new_val[type_checks],
                            function(x) .capture(class(x))
                        )
                    nchar_indices <-
                        max(c(nchar(fail_indices), 2))
                    nchar_classes <-
                        max(c(vapply(fail_classes, nchar, 2L), 7))

                    fmt_fun <-
                        function(id, class) {
                            sprintf(
                                paste0(
                                    "[%",
                                    nchar_indices,
                                    "s] %-",
                                    nchar_classes,
                                    "s"
                                ),
                                id,
                                class
                            )
                        }

                    sep_fun <-
                        function(symbol = "-") {
                            paste0(
                                c(
                                    rep(symbol, nchar_indices + 2),
                                    " ",
                                    rep(symbol, nchar_classes + 1)
                                ),
                                collapse = ""
                            )
                        }

                    stop(paste0(
                        private$.err_template(new_val, field_name),
                        "All elements in ",
                        field_name,
                        " musst be of class(es) ",
                        "\n",
                        .indent(
                            paste0("`", .capture(expected_type), "`"),
                            3
                        ),
                        "\n",
                        "but the following elements in the assignment are not:",
                        "\n",
                        paste0(
                            c(
                                .indent(
                                    c(
                                        fmt_fun("id", "classes"),
                                        sep_fun("-")
                                    ),
                                    3
                                ),
                                .cut_string(
                                    .indent(
                                        paste0(
                                            fmt_fun(fail_indices, fail_classes),
                                            collapse = "\n"
                                        ),
                                        3
                                    ),
                                    total_length = .warn_length_minus(400),
                                    append = "[...]"
                                )
                            ),
                            collapse = "\n"
                        )
                    ))
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > .err_* < ================================ #

            # ============================ > .err_template < ============================= #

            .err_template = function(new_val, field_name, len_after = 0) {
                err_msg <-
                    paste0(
                        "\rInvalid attempt to set\n",
                        .indent(
                            paste0(
                                "`",
                                private$.name,
                                "$",
                                field_name
                            ),
                            3
                        )
                    )

                assignment_msg <-
                    if (!missing(new_val)) {
                        paste(
                            " <-\n",
                            .capture(
                                new_val,
                                collapse = "\n"
                            )
                        )
                    } else {
                        NULL
                    }
                remaining_chars <-
                    .warn_length_minus(
                        nchar(err_msg) + nchar(assignment_msg) + 2 + len_after
                    )
                if (!field_name %in% c("restrictions")) {
                    # if (remaining_chars >= 0) {
                    err_msg <-
                        paste0(
                            err_msg,
                            .cut_string(
                                .indent(assignment_msg, 6, begin = FALSE),
                                total_length = .warn_length_minus(
                                    nchar(err_msg) +
                                        2 +
                                        len_after
                                ),
                                append = " [...]"
                            )
                        )
                    # }
                }
                err_msg <-
                    paste0(err_msg, "`\n")
                err_msg
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ====================== > .err_violated_restrictions < ====================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Throw error if restrictions are violated                               ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #
            .err_violated_restrictions = function(
                restriction_msgs,
                restrictions = private$.restrictions,
                value = private$.value,
                err_type = c("value", "restrictions")
            ) {
                err_type <-
                    match.arg(err_type)

                if (length(restriction_msgs) > 0) {
                    val_output <-
                        .capture(value, collapse = "\n")
                    val_sep <-
                        ifelse(nchar(val_output) > 25, "\n", " ")

                    message_restrictions <-
                        paste0(
                            paste0(
                                "- ",
                                restriction_msgs,
                                collapse = "\n"
                            )
                        )
                    err_msg_after <-
                        paste0(
                            switch(
                                err_type,
                                "value" = "Assignment",
                                "restrictions" = "Current value"
                            ),
                            " does not meet the following restriction",
                            if (length(restriction_msgs) > 1) {
                                "s"
                            },
                            ":\n",
                            .indent(message_restrictions, 3),
                            switch(
                                err_type,
                                value = "",
                                "restrictions" = paste(
                                    "\nSet value of",
                                    self$name,
                                    "to `NULL` before assigning the restrictions",
                                    "to prevent invalidating R6Member object."
                                )
                            )
                        )

                    err_msg <-
                        paste0(
                            private$.err_template(
                                new_val = get(err_type),
                                field_name = err_type,
                                len_after = nchar(err_msg_after)
                            ),
                            err_msg_after
                        )
                    stop(err_msg)
                }
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =============================== > .print_* < =============================== #

            # ============================= > .print_value < ============================= #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Print the current value of this member                                 ││ #
            # ││ Outputs the member's value to console.                                 ││ #
            # ││ Returns: NULL (invisibly)                                              ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .print_value = function() {
                cat(self$get())
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ========================= > .print_restrictions < ========================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Format restrictions as a comma-separated or newline-separated string   ││ #
            # ││ Returns formatted restriction labels, breaking to multiple lines       ││ #
            # ││ if the total width exceeds 80 characters.                              ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .print_restrictions = function() {
                restriction_labels <-
                    names(private$.restrictions)
                paste0(
                    restriction_labels,
                    sep = if (
                        (sum(nchar(restriction_labels)) +
                            (length(restriction_labels) - 1) * nchar(sep)) >
                            80
                    ) {
                        ",\n"
                    } else {
                        ", "
                    }
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > .set_* < ================================ #

            # ============================== > .set_name < =============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set the member's name and validate it                                  ││ #
            # ││ Ensures name is: not NULL, not empty, a valid R variable name          ││ #
            # ││ Name serves as identifier in R6Class definitions and declarations.     ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_name = function(name) {
                # Make sure name is not set to `NULL`
                if (is.null(name)) {
                    stop("Name must not be `NULL`!")
                }
                # Make sure name is not set to only whitespaces
                if (grepl("^\\s*$", name)) {
                    stop("Name must not be empty string!")
                }

                # Make sure name is a valid variable name

                if (make.names(name) != name) {
                    mkname <-
                        make.names(gsub(
                            "^[^A-Za-z]+",
                            "",
                            name,
                            perl = TRUE,
                            ignore.case = TRUE
                        ))

                    stop(paste0(
                        "Name must be a valid variable name.\n\tE.g. use '",
                        mkname,
                        "' instead."
                    ))
                }

                private$.name <-
                    name

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ========================== > .set_restrictions < =========================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set validation restrictions for the member's value                     ││ #
            # ││ Validates the current value against new restrictions before assigning. ││ #
            # ││ Empty list becomes NULL (no restrictions). Returns invisibly NULL.      ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_restrictions = function(restrictions) {
                if (length(restrictions) == 0) {
                    restrictions <-
                        NULL
                }

                private$.ensure_restrictions(
                    value = private$.value,
                    restrictions = restrictions,
                    err_type = "restrictions"
                )

                private$.restrictions <-
                    restrictions

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ============================== > .set_value < ============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set the member's value with restriction checking                       ││ #
            # ││ Validates value against all restrictions before assignment.            ││ #
            # ││ Automatically sets is_method based on whether value is a function.     ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_value = function(value) {
                private$.ensure_restrictions(
                    value = value,
                    err_type = "value"
                )

                private$.value <-
                    value

                private$.is_method <-
                    is.function(private$.value)
                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =========================== > .set_is_private < ============================ #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set whether the member is private in the R6 class                      ││ #
            # ││ Private members are not exposed in public interface.                    ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_is_private = function(value) {
                private$.is_private <-
                    value

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =========================== > .set_is_method < ============================ #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set whether the member should be treated as a method                   ││ #
            # ││ Typically auto-set when value changes. Can be overridden manually.     ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_is_method = function(value) {
                private$.is_method <-
                    value

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > .set_locked < ============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set locking state for all member fields                                ││ #
            # ││ When TRUE, prevents modification of member's fields after init.        ││ #
            # ││ Also sets value_locked to same state (can be overridden separately).   ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_locked = function(value) {
                private$.locked <-
                    private$.value_locked <-
                        value

                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ========================== > .set_value_locked < =========================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Set locking state for the value field only                             ││ #
            # ││ When TRUE, prevents modification of the value after initialization.    ││ #
            # ││ Provides fine-grained control independent of general locking state.    ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .set_value_locked = function(value) {
                private$.value_locked <-
                    value

                return(invisible(NULL))
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        ),
        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # =============================== > public < ================================ #
        public = list(
            # =============================== > methods < ================================ #
            # =============================== > accessor < =============================== #
            #' @details
            #' Access or modify one of the internal fields of `r .doc_ref_self()`s
            #'
            #' @param field_value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="field_value"}
            #' ```
            #'
            #' @param field_name
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="field_name"}
            #' ```
            #'
            #' @return
            #' ```{r,eval=TRUE,echo=FALSE,results="asis"}
            #' .doc_tbl(
            #'   table = rbind(
            #'     c(
            #'          "If `field_value` is **not** provided (getter case):",
            #'          paste(
            #'          "the field's current value -- see",.doc_ref_section("active bindings"), "for the fields' types."
            #'          )
            #'      ),
            #'      c(
            #'          "If `field_value` is provided (setter case):",
            #'          .doc_value(NULL)
            #'      )
            #'    )
            #' )
            #' ```
            accessor = function(field_value, field_name) {
                private$.accessor(field_value, field_name)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================== > initialize < ============================== #
            #' @details
            #' `r .doc_phrase_constructor()` for creating and initializing a new `r .doc_ref_self()` object.
            #' The constructor validates all provided arguments against the specified restrictions
            #' and locks the object after initialization.
            #'
            #' @param name
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="name"}
            #' ```
            #'
            #' @param value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value"}
            #' ```
            #'
            #' @param restrictions
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restrictions"}
            #' ```
            #'
            #' @param locked
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="locked"}
            #' ```
            #'
            #' @param value_locked
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value_locked"}
            #' ```
            #'
            #' @param is_private
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="is_private"}
            #' ```
            #'
            #' @param is_method
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="is_method"}
            #' ```
            #'
            #' @return
            #' `r .doc_ref_self()` object with all specified fields initialized and locked
            initialize = function(
                name,
                value = NULL,
                restrictions = NULL,
                locked = TRUE,
                value_locked = FALSE,
                is_private = FALSE,
                is_method = NULL
            ) {
                self$locked <-
                    FALSE
                self$restrictions <-
                    restrictions
                self$name <-
                    name
                if (!is.null(value)) {
                    self$value <-
                        value
                }
                if (!is.null(is_method)) {
                    self$is_method <-
                        is_method
                }
                self$is_private <-
                    is_private

                self$locked <-
                    locked
                self$value_locked <-
                    value_locked
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > print < ================================= #

            #' @details
            #' Print an `r .doc_ref_self()` object to the console.
            #' Displays the member's name, optionally its value, and optionally its restrictions.
            #' Useful for inspecting member definitions and their validation rules.
            #'
            #' @param value_level
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value_level"}
            #' ```
            #' When set to a positive number, this controls the maximum width for displaying the value.
            #' If `TRUE`, displays the full value. If `FALSE` or `0`, the value is not displayed.
            #'
            #' @param restriction_level
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restriction_level"}
            #' ```
            #' Controls the detail level when printing restrictions.
            #' Higher values show more detailed restriction information.
            #' `0` means restrictions are not printed.
            #'
            #' @param print_class
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="print_class"}
            #' ```
            #' When `TRUE`, the class name is prepended to the member name in the output.
            #'
            #' @return
            #' `r .doc_value("NULL")` (invisibly). Output is printed to the console.
            print = function(
                value_level = TRUE,
                restriction_level = 0,
                print_class = TRUE
            ) {
                if (is.na(value_level)) {
                    stop("`value_level` must not be NA!")
                }
                if (is.na(restriction_level)) {
                    stop("`restriction_level` must not be NA!")
                }
                if (is.na(print_class)) {
                    stop("`print_class` must not be NA!")
                }
                val_output <-
                    if (value_level > 0) {
                        val_str <-
                            .capture(self$value)
                        if (
                            is.finite(value_level) && !is.logical(value_level)
                        ) {
                            if (nchar(val_str) > value_level) {
                                val_str <-
                                    .cut_string(
                                        val_str,
                                        line_length = value_level,
                                        append = ""
                                    )
                                if (nchar(val_str) > 75) {
                                    val_str <-
                                        paste0(val_str, "\n...")
                                } else {
                                    val_str <-
                                        paste0(
                                            val_str,
                                            " ..."
                                        )
                                }
                            }
                        }
                        paste0(
                            .indent(
                                paste0(
                                    "\n",
                                    "Value:",
                                    "\n"
                                ),
                                3
                            ),
                            .indent(
                                paste0(
                                    "`",
                                    val_str,
                                    "`"
                                ),
                                3
                            )
                        )
                    } else {
                        NULL
                    }

                restr_output <-
                    NULL
                if (
                    (restriction_level > 0) &&
                        (length(self$restrictions) > 0)
                ) {
                    restr_output <-
                        paste0(
                            "\n",
                            .indent("restrictions", 3),
                            ":\n",
                            .indent(
                                paste0(
                                    Reduce(
                                        c,
                                        lapply(
                                            self$restrictions,
                                            function(x) {
                                                utils::capture.output(print(
                                                    x,
                                                    restriction_level = restriction_level
                                                ))
                                            }
                                        )
                                    ),
                                    collapse = "\n"
                                ),
                                5
                            )
                        )
                }
                cat(
                    if (print_class) {
                        paste0(
                            gsub(
                                "\"",
                                "",
                                .capture(setdiff(class(self), "R6"))
                            ),
                            " "
                        )
                    },
                    self$name,
                    val_output,
                    restr_output,
                    "\n",
                    sep = ""
                )
                return(invisible(NULL))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================== > declare_* < =============================== #

            # ======================== > declare_active_binding < ======================== #

            #' @details
            #' Create `r .doc_r6class_activeBinding()` `r .doc_type_function()`  for `r .doc_ref_self()` object.
            #' `r .doc_fmt_highlight(.doc_phrase_r6declaring())`,
            #' see also
            #' the
            #' `r .doc_ref_method(topic="R6MemberList",tag="R6Class")`
            #' method
            #' and
            #' `r .doc_ref_help(topic="r6class")`.
            #'
            #' @return
            #' `r .doc_type_function()`
            #' to be used in the
            #' `r .doc_r6class_activeBinding(suffix="")`
            #' argument of
            #' `r .doc_r6class(suffix="()")`.
            declare_active_binding = function() {
                if (private$.is_method || private$.is_private) {
                    return(invisible(NULL))
                }
                accessor <-
                    function(new_value) {}
                body(accessor) <-
                    parse(
                        text = paste0(
                            private$.pos_in_parent("private"),
                            "$accessor(new_value,'value')"
                        )
                    )
                return(setNames(
                    list(accessor),
                    self$name
                ))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================ > declare_method < ============================ #

            #' @details
            #' Create  `r .doc_r6class_method(suffix=" function")` for `r .doc_ref_self()` object.
            #' `r .doc_fmt_highlight(.doc_phrase_r6declaring())`,
            #' see also
            #' the
            #' `r .doc_ref_method(topic="R6MemberList",tag="R6Class")`
            #' method
            #' and
            #' `r .doc_ref_help(topic="r6class")`.
            #'
            #' @return
            #' `r .doc_type_function()`
            #' to be used in the
            #' `r .doc_r6class_method()`
            #' argument of
            #' `r .doc_r6class(suffix="()")`.
            declare_method = function() {
                if (!private$.is_method || private$.is_private) {
                    return(invisible(NULL))
                }
                method <-
                    function() {}

                formals(method) <-
                    formals(private$.value)
                body(method) <-
                    parse(
                        text = paste0(
                            private$.pos_in_parent("private"),
                            "$value(\n",
                            paste0(
                                "\t",
                                names(formals(method)),
                                "=",
                                names(formals(method)),
                                collapse = ",\n"
                            ),
                            "\n)"
                        )
                    )
                return(setNames(
                    list(method),
                    self$name
                ))
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ======================== > declare_initialization < ======================== #

            #' @details
            #' Create (initial) assignment `r .doc_type_expression()` for
            #' `r .doc_ref_self()` object.
            #' `r .doc_fmt_highlight(.doc_phrase_r6declaring())`,
            #' see also
            #' the
            #' `r .doc_ref_method(topic="R6MemberList",tag="R6Class")`
            #' method
            #' and
            #' `r .doc_ref_help(topic="r6class")`.
            #'
            #' @return
            #' `r .doc_type_expression()` containing the assignment statement
            #' to be used in the
            #' `r .doc_r6class_public(suffix="[[\"initialize\"]]")`
            #' (constructor)
            #' method of
            #' `r .doc_r6class(suffix="()")`.
            declare_initialization = function() {
                if (private$.value_locked || private$.is_private) {
                    return(invisible(NULL))
                }
                parse(
                    text = paste0(
                        if (!private$.is_method) {
                            private$.pos_in_parent("public")
                        } else {
                            paste0(private$.pos_in_parent("private"), "$value")
                        },
                        " <- `",
                        self$name,
                        "`"
                    )
                )
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ────────────────────────────────── <end> ─────────────────────────────────── #
        ),
        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # ================================ > active < ================================ #

        active = list(
            # ============================== > is_method < =============================== #

            #' @field is_method
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="is_method"}
            #' ```
            is_method = function(new_is_method) {
                private$.accessor(new_is_method, "is_method")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================== > is_private < ============================== #

            #' @field is_private
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="is_private"}
            #' ```
            is_private = function(new_is_private) {
                private$.accessor(new_is_private, "is_private")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > locked < ================================ #

            #' @field locked
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="locked"}
            #' ```
            locked = function(new_locked) {
                private$.accessor(new_locked, "locked")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================= > name < ================================= #

            #' @field name
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="name"}
            #' ```
            name = function(new_name) {
                private$.accessor(new_name, "name")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > private_name < ============================= #

            #' @field private_name
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="private_name"}
            #' ```
            private_name = function(new_name) {
                private$.accessor(new_name, "private_name")()
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > restrictions < ============================= #

            #' @field restrictions
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="restrictions"}
            #' ```
            restrictions = function(new_restrictions) {
                private$.accessor(new_restrictions, "restrictions")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > value < ================================= #

            #' @field value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value"}
            #' ```
            value = function(new_value) {
                private$.accessor(new_value, "value")
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ============================= > value_locked < ============================= #

            #' @field value_locked
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="value_locked"}
            #' ```
            value_locked = function(new_locked) {
                private$.accessor(new_locked, "value_locked")
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        )
        # ────────────────────────────────── <end> ─────────────────────────────────── #
    )

# ────────────────────────────────── <end> ─────────────────────────────────── #
