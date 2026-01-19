R6MemberRestriction_Sum <-
    R6MemberRestriction$new(
        id = "Sum",
        restriction = function(x) {
            sum(x) == 5
        }
    )

R6MemberRestriction_Len <-
    R6MemberRestriction$new(
        id = "Length",
        restriction = function(x) {
            if (length(x) > 2) stop("Length should be <= 2!")
        }
    )

R6MemberRestriction_Len3 <-
    R6MemberRestriction$new(
        id = "Length 3",
        restriction = function(x) {
            if (length(x) != 3) stop("Length should be == 5!")
        }
    )

R6MemberRestriction_Len5 <-
    R6MemberRestriction$new(
        id = "Length",
        restriction = function(x) {
            if (length(x) != 5) stop("Length should be == 5!")
        }
    )

R6MemberRestriction_Diff <-
    R6MemberRestriction$new(
        id = "Diff",
        restriction = function(x) {
            if (length(x) == 2) {
                diff(x) == 3
            } else {
                stop("Invalid lengt of x!")
            }
        }
    )

R6MemberRestriction_Diff_invalid <-
    R6MemberRestriction$new(
        id = "Diff",
        restriction = function(x) {
            if (length(x) == 2) {
                diff(x) == 4
            } else {
                stop("Invalid lengt of x!")
            }
        }
    )

R6MemberRestriction_is_integer <-
    R6MemberRestriction$new(
        id = "Is Integer",
        restriction = function(obj) {
            is.integer(obj) ||
                (is.numeric(obj) && (all.equal(as.integer(obj), obj)))
        }
    )

R6MemberRestriction_is_numeric <-
    R6MemberRestriction$new(
        id = "Is Numeric",
        restriction = function(obj) {
            is.numeric(obj)
        }
    )

R6MemberRestriction_is_list <-
    R6MemberRestriction$new(
        id = "Is List",
        restriction = function(obj) {
            "list" %in% class(obj)
        }
    )

R6MemberRestriction_is_function <-
    R6MemberRestriction$new(
        id = "Is Function",
        restriction = is.function
    )

R6MemberRestriction_takes_1_arg <-
    R6MemberRestriction$new(
        id = "Is Function",
        restriction = function(obj) {
            if(!is.function(obj)){
                stop("Not a function!")
            }
            length(formals(obj)) == 1
        }
    )

R6MemberRestriction_is_named <-
    R6MemberRestriction$new(
        id = "Is Named",
        restriction = function(obj) {
            length(names(obj)) == length(obj)
        }
    )
