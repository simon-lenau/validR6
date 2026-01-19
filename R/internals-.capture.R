# =============================== > .capture < =============================== #
#'
#' @title .capture
#'
#' @details
#'  Format an object in a printable form
#'
#' @param obj
#'  (any)
#'  The object to print
#'
#' @param collapse
#' ```{r,eval=TRUE,echo=FALSE,results="asis"}
#' .doc_arg(
#'     type = "character",
#'     description = c(
#'          "If multiple lines are printed: The character to collapse them using",
#'          .doc_ref(namespace="base",topic="paste0"),
#'          "."
#'     ),
#'     default = " "
#' )
#' ```
#'
#' @return
#'  A string representing the object
#'
#' @examplesIf interactive()
#'  .capture(c(1:5,33), collapse=" ")
#'  .capture(function(x) x+1, collapse=" ")
#'
#' @keywords internal
#' @noRd

.capture <-
    function(obj, collapse = " ") {
        return(paste0(utils::capture.output(dput(obj)), collapse = collapse))
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
