# ============================= > SomeR6Class < ============================== #

SomeR6ClassDefinitionList <-
    R6MemberList$new(
        R6Member$new(
            name = "attr_list",
            restrictions = list(
                R6MemberRestriction_is_list,
                R6MemberRestriction_is_named
            ),
            value = list(a = 1:3, b = 4),
            locked = FALSE
        ),
        R6Member$new(
            name = "attr_int",
            restrictions = list(
                R6MemberRestriction_is_integer,
                R6MemberRestriction_Len3
            ),
            value = 1:3,
            locked = FALSE
        ),
        R6Member$new(
            name = "private_attr_num",
            restrictions = list(
                R6MemberRestriction_is_numeric
            ),
            value = 1:3,
            locked = FALSE,
            is_private = TRUE
        ),
        R6Member$new(
            name = "mtd_multiply",
            restrictions = list(
                R6MemberRestriction_is_function,
                R6MemberRestriction_takes_1_arg
            ),
            value = function(x) x * self$attr_int,
            locked = FALSE
        ),
        R6Member$new(
            name = "fun_list_elem",
            is_method = FALSE,
            restrictions = list(
                R6MemberRestriction_is_function
            ),
            value = function(x) self$attr_list[[x]],
            locked = FALSE
        )
    )


SomeR6Class <-
    SomeR6ClassDefinitionList$R6Class(
        "SomeR6Class"
    )

# ────────────────────────────────── <end> ─────────────────────────────────── #
