#' Paste with Line Width Constraint
#'
#' Pastes character elements together, using space as separator by default.
#' If the resulting line exceeds the specified width, uses newline as separator instead.
#'
#' @param ... Character vectors or elements to paste together
#' @param sep_normal Separator to use when line width is not exceeded (default: " ")
#' @param linewidth Maximum line width in characters (default: 80)
#'
#' @return A character string with elements pasted together
#' @noRd
.paste_to_linewidth <- function(
    ...,
    sep_normal = " ",
    linewidth = getOption("width")
) {
    # Convert all arguments to character and flatten any vectors
    elements <- as.character(unlist(list(...)))

    if (length(elements) == 0) {
        return("")
    }

    # Calculate character counts
    nchars <- nchar(elements)
    sep_len <- nchar(sep_normal)

    # Determine where line breaks are needed
    # A break is needed before element i if adding it would exceed linewidth
    needs_break <- c(
        FALSE,
        diff((cumsum(nchars + sep_len)) %% linewidth) < 0
    )

    # Choose separator based on needs_break
    separators <- ifelse(needs_break, "\n", sep_normal)

    # Construct result by pasting elements with separators
    result <-
        paste0(elements, c(separators[-1], "")) |> paste(collapse = "")
}
