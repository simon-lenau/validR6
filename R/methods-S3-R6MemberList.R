# ================================= > `[` < ================================== #
#' @export
`[.R6MemberList` <-
    function(x, i, ...) {
        x$select(i, inner = FALSE)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > `[<-` < ================================= #
#' @export
`[<-.R6MemberList` <-
    function(x, i, value) {
        x$select(i, new_value = value, inner = FALSE)
        return(x)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================= > `[[` < ================================= #
#' @export
`[[.R6MemberList` <-
    function(x, i, ...) {
        x$select(i, inner = TRUE)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > `[[<-` < ================================ #
#' @export
`[[<-.R6MemberList` <-
    function(x, i, value) {
        x$select(i, new_value = value, inner = TRUE)
        return(x)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================= > `c` < ================================== #
#' @export
`c.R6MemberList` <-
    function(...) {
        objects <- list(...)
        
        if (length(objects) == 0) {
            return(NULL)
        }
        
        if (length(objects) == 1) {
            return(objects[[1]])
        }
        
        # Start with the first object and combine all others
        result <- objects[[1]]
        for (i in 2:length(objects)) {
            result <- result$combine(objects[[i]])
        }
        
        return(result)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
