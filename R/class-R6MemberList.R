#' @import R6
# ================================ > Title < ================================= #

#' @title
#' `r .doc_topic_set("R6MemberList")`
#' `r .doc_ref_self()`:
#' Wrapping toolbox for using
#' `r .doc_r6member(suffix="s",link=FALSE)`
#' in `r .doc_r6class(suffix="")` definitions.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > Description < ============================== #
#' @description
#' A `r .doc_ref_self()` provides utility
#' `r .doc_ref_section("methods",tag="active_bindings")`
#' for declaring
#' `r .doc_r6class()` with members wrapped in
#' `r .doc_r6member()` objects.
#' As such, the `r .doc_ref_self()` encloses zero or more
#' `r .doc_r6member()` objects that
#' define the members in
#' a `r .doc_r6class(suffix="")`.
#' <br>
#' The core method for this purpose is `r .doc_ref_method(tag="R6Class")`.
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Details < ================================ #
#' @section `r .doc_section("Details")`
#' Once a `r .doc_ref_self()` is instanciated,
#' the
#'  `r .doc_ref_method(tag="R6Class")`
#' method can be used
#' to define a `r .doc_r6class(suffix="")`
#' and delegate the enclosed
#' `r .doc_r6member()` objects
#' as members to this new `r .doc_r6class(suffix="")`.
#'
#' The core method for this purpose is `r .doc_ref_method(tag="R6Class")`,
#' which calls the worker methods
#' `r .doc_fmt_cslist(.doc_ref_method(tag="public_list"), .doc_ref_method(tag="private_list"), .doc_ref_method(tag="active_bindings"))`.
#' These set up the `r .doc_type_list(suffix="s")` defining the arguments
#' `r .doc_fmt_cslist(.doc_ref(tag="public",topic="R6Class",namespace="R6"), .doc_ref(tag="private",topic="R6Class",namespace="R6"), .doc_ref(tag="active",topic="R6Class",namespace="R6"))`
#' for the call to `r .doc_r6class(suffix="()",prefix="R6::")`.
#'
#' ## `r .doc_section(section="Retrieving private and public names",tag="details-names",level=2)`
#' Use `r .doc_ref_method(tag="names")` to retrieve public or private member names
#' of the `r .doc_r6member()` objects in the `r .doc_r6class(suffix="")`
#' created by `r .doc_ref_method(tag="R6Class")`.
#'
#' ## `r .doc_section(section="Subsetting", tag = "details-subsetting",level=2)`
#' The
#' `r .doc_ref_method(tag="select")` method
#' allows access to the enclosed `r .doc_r6member()` objects
#' by name.
#' Alternatively, the `r .doc_method_s3(text="S3-operators")`
#' `r .doc_fmt_cslist(.doc_method_sqbr(), .doc_method_sqbrsqbr())`
#' allow subsetting for `r .doc_ref_self()` as well.
#'
#' See the
#' `r .doc_ref_section("methods")` and `r .doc_ref_section("examples")`
#' section for more details and examples.
# ────────────────────────────────── <end> ─────────────────────────────────── #
# =============================== > Examples < =============================== #
#' @section `r .doc_section("Examples")`
#' ```{r child = "man-roxygen/examples/class-R6MemberRestriction-initialize.Rmd"}
#' ```
#' ```{r child = "man-roxygen/examples/class-R6Member-initialize-valid.Rmd"}
#' ```
#' ```{r child = "man-roxygen/examples/class-R6MemberList-initialize.Rmd"}
#' ```
#'
# ────────────────────────────────── <end> ─────────────────────────────────── #
#' @export

R6MemberList <-
    R6::R6Class(
        "R6MemberList",
        # =============================== > private < ================================ #

        private = list(
            # ================================ > fields < ================================ #
            .members = NULL,
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =============================== > methods < ================================ #
            # ============================ > .member_apply < ============================= #

            .member_apply = function(
                fun,
                subset = c("all", "public", "private"),
                indices
            ) {
                if (length(formals(fun)) != 1) {
                    stop(
                        "`.member_apply` works only with functions taking exactly one element!"
                    )
                }

                subset <-
                    match.arg(subset)

                if (missing(indices)) {
                    if (subset == "all") {
                        member_indices <-
                            seq_len(length(private$.members))
                    } else {
                        members_private <-
                            vapply(
                                private$.members,
                                function(i) i$is_private,
                                TRUE
                            )
                        if (subset == "public") {
                            member_indices <-
                                which(!members_private)
                        }
                        if (subset == "private") {
                            member_indices <-
                                which(
                                    members_private
                                )
                        }
                    }
                } else {
                    if (!is.character(indices)) {
                        stop(
                            "Subsetting indices must be names!"
                        )
                    }
                    indices_check <-
                        indices %in% names(private$.members)
                    if (!all(indices_check)) {
                        stop(paste0(
                            "The following indices are not names of list members:\n\t",
                            .cut_string(.capture(indices[!indices_check]))
                        ))
                    }
                    member_indices <-
                        indices
                }

                if (length(member_indices) == 0) {
                    return(list())
                }
                lapply(
                    private$.members[member_indices],
                    fun
                )
            },

            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ============================== > .get_names < ============================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Get names from R6Members in the list                                   ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .get_names = function(
                type = c("public", "private"),
                subset = "all"
            ) {
                type <-
                    match.arg(type)
                output <-
                    Reduce(
                        c,
                        private$.member_apply(
                            subset = subset,
                            fun = if (type == "public") {
                                function(x) x$name
                            } else {
                                function(x) x$private_name
                            }
                        )
                    )
                output
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ======================= > .make_field_assignments < ======================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Declare initialization statements for R6 constructor                   ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .make_field_assignments = function() {
                Reduce(
                    c,
                    private$.member_apply(
                        fun = function(x) {
                            x$declare_initialization()
                        },
                        subset = "public"
                    )
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #

            # ======================= > .make_method_initialize < ======================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Declare initialization method for creating R6 Classes                  ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .make_method_initialize = function() {
                method_initialize <-
                    function() {}

                # Match the constructor's arguments match the list elements' name
                formals(method_initialize) <-
                    setNames(
                        private$.member_apply(
                            fun = function(x) {
                                parse(text = .capture(x$value))[[1]]
                            },
                            subset = "public"
                        ),
                        private$.get_names("public", subset = "public")
                    )

                # Define the constructor's body to pass all arguments to the
                #   R6Members
                body(method_initialize) <-
                    parse(
                        text = c(
                            "{",
                            # Assignments in initialize method
                            private$.make_field_assignments(),
                            # private$.make_env_assignments(),
                            "}"
                        )
                    )
                return(method_initialize)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ========================= > .make_method_print < ========================== #

            # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
            # ││ Declare private method for printing all R6 Members in                  ││ #
            # ││ the R6 Class to be defined                                             ││ #
            # └└────────────────────────────────────────────────────────────────────────┘┘ #

            .make_method_print = function() {
                .print_members_fun <-
                    function() {
                        cat(
                            "Object of class(es)\n",
                            .indent(
                                paste0(
                                    "`",
                                    .capture(setdiff(class(self), "R6"))
                                ),
                                3
                            ),
                            "`\nMembers:\n",
                            sep = ""
                        )
                        # Pass all valid arguments to the print method of `R6Member`
                        args <-
                            as.list(match.call())[-1]
                        args <-
                            args[
                                names(args) %in%
                                    names(formals(
                                        R6Member$public_methods$print
                                    ))
                            ]

                        # Loop over all `R6Member`s and print them
                        for (i in (private$`__R6Members__`$private)) {
                            cat(
                                .indent(
                                    space = prefix,
                                    times = 1,
                                    text = gsub(
                                        "($|\\n)",
                                        paste0(suffix, "\\1"),
                                        paste0(
                                            utils::capture.output(
                                                do.call(
                                                    "print",
                                                    c(
                                                        list(
                                                            x = private[[i]]
                                                        ),
                                                        args
                                                    )
                                                )
                                            ),
                                            collapse = "\n"
                                        )
                                    )
                                ),
                                "\n"
                            )
                        }
                        cat("\n")
                        return(invisible(NULL))
                    }

                # Match R6Member$print() arguments and defaults
                formals(.print_members_fun) <-
                    c(
                        formals(
                            R6Member$public_methods$print
                        ),
                        list(prefix = "", suffix = "")
                    )

                .print_members_fun
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        ),
        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # ────────────────────────────────── <end> ─────────────────────────────────── #
        # ================================ > public < ================================ #

        public = list(
            # =============================== > methods < ================================ #

            # ============================== > initialize < ============================== #
            #' @description
            #' `r .doc_phrase_constructor()`
            #'  from the supplied `r .doc_r6member()` objects.
            #'
            #' @param ...
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="dots"}
            #' ```
            #'
            #' @return
            #' `r .doc_ref_self()` object containing the provided
            #' `r .doc_r6member()` objects.

            initialize = function(...) {
                # Check if all arguments are of type `R6Member`
                R6Member_check <-
                    vapply(
                        seq_len(...length()),
                        function(i) {
                            "R6Member" %in% class(...elt(i))
                        },
                        TRUE
                    )
                if (!all(R6Member_check)) {
                    stop(paste0(
                        "The following elements are not of type 'R6Member':\n\t",
                        .cut_string(.capture(...names()[!R6Member_check]))
                    ))
                }
                # Store input `R6Member` objects as members in list
                private$.members <-
                    list(...)

                # Name members list
                names(private$.members) <-
                    private$.get_names()
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =========================== > active_bindings < ============================ #
            #'
            #' @description
            #' Create `r .doc_r6memberlist_outputMethods("active")`
            #'
            #' @return
            #' A `r .doc_r6memberlist_outputMethods("active",ref_help=FALSE)`

            active_bindings = function() {
                Reduce(
                    c,
                    lapply(private$.members, function(x) {
                        x$declare_active_binding()
                    })
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # =============================== > R6Class < ================================ #

            #'
            #' @description
            #' Declare `r .doc_r6class(suffix="")` from `r .doc_ref_self()`
            #'
            #' @param classname
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="classname"}
            #' ```
            #'
            #' @param print_method
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="print_method"}
            #' ```
            #'
            #' @return
            #' A `r .doc_r6class(suffix="")` with members defined by the `r .doc_ref_self()`
            #'
            R6Class = function(
                classname,
                print_method = "default"
            ) {
                if (missing(classname)) {
                    classname <-
                        NULL
                }
                R6::R6Class(
                    classname,
                    private = self$private_list(),
                    public = self$public_list(
                        print_method = print_method
                    ),
                    active = self$active_bindings()
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > public_list < ============================== #

            #' @description
            #' Create `r .doc_r6memberlist_outputMethods("public")`
            #'
            #' @param print_method
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="print_method"}
            #' ```
            #'
            #' @return
            #' A `r .doc_r6memberlist_outputMethods("public",ref_help=FALSE)`

            public_list = function(print_method = "default") {
                print_fun <-
                    if (
                        is.character(print_method) &&
                            (print_method == "default")
                    ) {
                        private$.make_method_print()
                    } else if (
                        is.character(print_method) &&
                            (print_method == "none")
                    ) {
                        NULL
                    } else if (
                        is.function(print_method) ||
                            is.null(print_method)
                    ) {
                        print_method
                    } else {
                        stop(paste(
                            "Argument `print_method` should be 'default', 'none', NULL or function, but is ",
                            if (is.atomic(print_method)) {
                                paste0("`", .capture(print_method), "`")
                            } else {
                                paste0(
                                    "type `",
                                    .capture(class(print_method)),
                                    "`"
                                )
                            },
                            "."
                        ))
                    }

                c(
                    list(
                        initialize = private$.make_method_initialize()
                    ),
                    if (!is.null(print_fun)) {
                        list(
                            print = print_fun
                        )
                    },
                    Reduce(
                        c,
                        lapply(private$.members, function(x) {
                            x$declare_method()
                        })
                    )
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ============================= > private_list < ============================= #

            #' @description
            #' Create `r .doc_r6memberlist_outputMethods("private")`
            #'
            #' @return
            #' A `r .doc_r6memberlist_outputMethods("private",ref_help=FALSE)`

            private_list = function() {
                c(
                    # List containing member names
                    list(
                        "__R6Members__" = list(
                            public = private$.get_names("public"),
                            private = private$.get_names("private")
                        )
                    ),
                    # List containing R6Member objects
                    setNames(
                        lapply(private$.members, function(x) x$clone()),
                        private$.get_names("private")
                    ),
                    # Auxiliary functions
                    ## For printing R6Member objects
                    list(".print" = private$.make_method_print())
                )
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > print < ================================= #

            #' @description
            #' Print method for `r .doc_r6memberlist()`
            print = function() {
                print(private$.members)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > names < ================================= #

            #' @description
            #' `names` method for `r .doc_r6memberlist()`
            #'
            #' @param type
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="type"}
            #' ```
            #'
            #' @param subset
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="subset"}
            #' ```
            #'
            #' @return
            #' Character vector of names

            names = function(type = "public", subset = "all") {
                private$.get_names(type = type, subset = subset)
            },
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ================================ > select < ================================ #

            #' @description
            #' Subsetting method for `r .doc_r6memberlist()`
            #'
            #' @param member
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="member"}
            #' ```
            #'
            #' @param new_value
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="new_value"}
            #' ```
            #'
            #' @param inner
            #' ```{r child = "man-roxygen/fields_arguments/_template.Rmd",.doc_arg_name="inner"}
            #' ```
            #'
            #' @return
            #' select
            select = function(member, new_value, inner = TRUE) {
                if (missing(new_value)) {
                    if (inner) {
                        private$.members[[member]]
                    } else {
                        constructor <-
                            R6MemberList$new

                        do.call("constructor", private$.members[member])
                    }
                } else {
                    if (inner) {
                        private$.members[[member]]$value <-
                            new_value
                    } else {
                        if (length(member) != length(new_value)) {
                            stop(paste0(
                                "Length of indices (",
                                length(member),
                                ") != length of values (",
                                length(new_value),
                                ")."
                            ))
                        }
                        for (i in 1:length(member)) {
                            private$.members[[member[i]]]$value <-
                                new_value[[i]]
                        }
                    }
                }
            }
            # ────────────────────────────────── <end> ─────────────────────────────────── #
            # ────────────────────────────────── <end> ─────────────────────────────────── #
        )
        # ────────────────────────────────── <end> ─────────────────────────────────── #
    )
