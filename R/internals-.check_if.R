# ============================== > .check_if < =============================== #

#'
#' @include internals-.capture.R
#'
#' @title Construct check functions for given attributes
#'
#' @details
#'  Construct check functions for given attributes
#'
#' @param attr
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'          "The name of the attribute (`function`) to check."
#'     )
#' )
#' ```
#'
#' @param target
#'  (any)
#'  The target value of to check the attribute against
#'
#' @param comparison
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "function",
#'     description = c(
#'           "A function taking two arguments to compare the attribute and target"
#'     ),
#'     default = all.equal
#' )
#' ```
#'
#' @return
#'  A `function` taking a value and comparing it to `target`
#'
#' @examplesIf interactive()
#'  .check_if("length", 5, comparison=all.equal)(1:5)
#'  .check_if("length", 5, comparison=all.equal)(1:3)
#'
#' @keywords internal
#' @noRd

.check_if <-
    function(attr, target, comparison = all.equal) {
        compare_function <-
            function(value) {
                attr_function <-
                    get(attr, envir = globalenv(), mode = "function")
                if (!is.function(attr_function)) {
                    stop(paste0(
                        "`",
                        attr,
                        "` is not a function in globalenv()"
                    ))
                }
                val <-
                    attr_function(value)

                if (any(comparison(val, target) != TRUE)) {
                    err <-
                        (paste0(
                            "Comparing\n\t`",
                            attr,
                            "(",
                            .capture(value),
                            ")",
                            "`\n\t = ",
                            .capture(val),
                            "\nand\n\t`",
                            .capture(target),
                            "`\nusing\n\t`",
                            .capture(comparison_str),
                            "`"
                        ))
                    stop(err)
                }
                return(TRUE)
            }

        # Retrieve a printable (unexpanded) version of `comparison`
        #   and pass it to the created function's environment
        comparison_str <-
            match.call()$comparison
        # If comparison is not provided,
        #   the default needs to be obtained this way:
        if (is.null(comparison_str)) {
            comparison_str <-
                (substitute(comparison))
        }

        environment(compare_function)$comparison_str <-
            comparison_str

        compare_function
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
.check_if_function <-
    list("Must be function" = .check_if("class", "function"))

.check_if_integer <-
    list("Must be integer" = .check_if("class", "integer"))

.check_if_numeric <-
    list("Must be numeric" = .check_if("class", "numeric"))

.check_if_list <-
    list("Must be list" = .check_if("class", "list", function(x, y) x %in% y))

check_length <-
    function(len) {
        if (length(len) != 1) {
            stop("Argument 'len' should be length 1!")
        }
        if (
            (!"integer" %in% class(len)) &
                (!all(all.equal(len, as.integer(len)) == TRUE))
        ) {
            stop("Argument 'len' should be integer!")
        }
        output <-
            list(.check_if("length", len))
        names(output) <-
            paste("Must be length", len)
        output
    }

check_arg_names <-
    function(argnames, optional = NULL) {
        if (missing(argnames)) {
            return(list(
                "Must not have arguments" = .check_if(
                    "formalArgs",
                    NULL
                )
            ))
        }
        if (is.null(argnames) || all(argnames %in% "")) {
            return(list(
                "Must not have arguments" = .check_if(
                    "formalArgs",
                    NULL
                )
            ))
        }
        output <-
            list(
                .check_if(
                    "formalArgs",
                    argnames,
                    function(x, y) all(y %in% x)
                ),
                .check_if(
                    "formalArgs",
                    c(argnames, optional),
                    function(x, y) all(x %in% y)
                )
            )
        names(output) <-
            c(
                paste("Must have arguments", .capture(argnames)),
                if (!is.null(optional)) {
                    paste(
                        "May have additional arguments",
                        .capture(optional)
                    )
                } else {
                    "Must not have further arguments"
                }
            )
        output
    }
