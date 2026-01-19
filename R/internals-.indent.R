# =============================== > .indent < ================================ #

#' @title .indent
#'
#' @details
#' Indent text by repeating a (space) string a number of times
#'
#' @param text
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'          "The text to be indented."
#'     )
#' )
#' ```
#'
#' @param times
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "integer",
#'     description = c(
#'          "The number of times `space` is repeated."
#'     ),
#'     default = 0
#' )
#' ```
#' 
#'
#' @param space
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'          "The string that is inserted for indentation."
#'     ),
#'     default = ""
#' )
#' ```
#' 
#'
#' @return
#' A `r .doc_type_character()` containing indented text
#'
#' @examplesIf interactive()
#' .indent("ABC", times=1, space="\t")
#'
#' @keywords internal
#' @noRd

.indent <-
    function(text, times = 0, space = " ", begin = TRUE) {
        gsub(
            if (begin) {
                "(^|\n)"
            } else {
                "(\n)"
            },
            paste0(c("\\1", rep(space, times)), collapse = ""),
            text
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
