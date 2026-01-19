# ============================= > .make_label < ============================== #

#' @title .make_label
#'
#' @details
#' Make labels for printing in `testthat``
#'
#' @param ...
#' Strings to be concatenated
#'
#' @return 
#' Collapsed string of arguments
#'
#' @examplesIf interactive()
#' .make_label("A","B","C")
#'
#' @keywords internal
#' 
#' @noRd

.make_label <-
    function(...) {
        paste0(
            unlist(c(getOption("test_section"), ...), recursive = TRUE),
            collapse = " -> "
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
