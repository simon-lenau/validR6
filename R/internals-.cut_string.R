#' @title .cut_string
#'
#' @details
#' Declaration for `.cut_string`
#'
#' @param text
#' The string(s) to cut
#'
#' @param length
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "integer",
#'     description = c(
#'           "The maximum length of the string to return"
#'     ),
#'     default = 75
#' )
#' ```
#'
#' @param append
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'           "Text to append at the end of shortened string (parts)"
#'     ),
#'     default = " ..."
#' )
#' ```
#'
#' @param cut_at
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'           "Text to append at the end of shortened string (parts)"
#'     ),
#'     default = r"--(`c("\\s", ", ", ";")`)--"
#' )
#' ```
#'
#' @return  
#' `r .doc_type_character(text="strings")` no longer than `length`
#'
#' @examplesIf interactive()
#' .cut_string(text, length, append = "...")
#'
#' @keywords Internal
#' @noRd
.cut_string <-
    function(
        text,
        line_length = getOption("width"),
        total_length = .warn_length_minus(0),
        append = " ...",
        cut_at = c("\\s", ",", ";")
    ) {
        nchar_type <-
            "chars"
        if (is.finite(line_length)) {
            text_split <-
                strsplit(text, "\n")

            text_split <-
                lapply(text_split, function(elems) {
                    overlong_elems <-
                        nchar(elems, type = nchar_type) > line_length
                    if (any(overlong_elems)) {
                        elems[overlong_elems] <-
                            vapply(
                                elems[overlong_elems],
                                .cut_string_elem,
                                character(1),
                                length = line_length,
                                append = append,
                                cut_at = cut_at,
                                USE.NAMES = FALSE
                            )
                    }
                    elems
                })
            output <-
                vapply(
                    text_split,
                    paste0,
                    character(1),
                    collapse = "\n",
                    USE.NAMES = FALSE
                )
        } else {
            output <-
                text
        }

        if (is.finite(total_length)) {
            overlong_elems <-
                nchar(output, type = nchar_type) > total_length
            if (any(overlong_elems)) {
                output[overlong_elems] <-
                    vapply(
                        output[overlong_elems],
                        function(elem) {
                            paste0(
                                substr(
                                    gsub(
                                        paste0(
                                            "\\s*([\\}\\]\\)]*)\\s*",
                                            append,
                                            "\\s*([\\}\\]\\)]*\\s*$)"
                                        ),
                                        "\\1\\2",
                                        elem
                                    ),
                                    start = 1,
                                    stop = total_length -
                                        nchar(append, type = nchar_type)
                                ),
                                append
                            )
                        },
                        FUN.VALUE = character(1)
                    )
            }
        }

        return(output)
    }


#' @title .cut_string_elem
#'
#' @details
#' Declaration for `.cut_string_elem`
#'
#' @inheritParams .cut_string
#'
#' @return A string with length <= length
#'
#' @examplesIf interactive()
#' .cut_string_elem(text, length=75, append="...", cut_at=c("\\s", ", ", ";"))
#'
#' @keywords Internal
#' @noRd
.cut_string_elem <-
    function(
        text,
        length = getOption("width"),
        append = " ...",
        cut_at = c("\\s", ",", ";"),
        nchar_type = "chars"
    ) {
        if (nchar(text, type = nchar_type) <= length) {
            return(text)
        }
        whitespace_pos <-
            gregexec(
                paste0(c("[", cut_at, "]"), collapse = ""),
                text,
                perl = TRUE
            )[[1]]
        length <-
            length - nchar(append, type = nchar_type)
        cutpoint <-
            if (any(whitespace_pos <= length)) {
                max(whitespace_pos[whitespace_pos <= length])
            } else {
                length
            }

        if ((length - cutpoint) > length / 3) {
            cutpoint <-
                length
        }

        return(paste0(substr(text, 1, cutpoint), append))
    }

.warn_length_minus <-
    function(i) {
        floor(getOption("warning.length") * 0.99) - i
    }
