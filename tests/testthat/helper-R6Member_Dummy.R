source(file.path(testthat::test_path(), "helper-00.R"))
# ============================ > R6Member_Dummy < ============================ #

restriction_list <-
    list(
        sum = R6MemberRestriction_Sum,
        length = R6MemberRestriction_Len
    )

R6Member_Dummy <-
    R6Member$new(
        name = "R6Member_Dummy",
        restrictions = restriction_list,
        value = c(1, 4),
        locked = FALSE
    )
R6Member_Dummy_original <-
    R6Member_Dummy$clone()
# ────────────────────────────────── <end> ─────────────────────────────────── #
# =========================== > R6Member_Dummy2 < ============================ #

restriction_list2 <-
    list(
        sum = R6MemberRestriction_Sum,
        length = R6MemberRestriction_Len5
    )

R6Member_Dummy2 <-
    R6Member$new(
        name = "R6Member_Dummy2",
        restrictions = restriction_list2,
        value = rep(1, 5),
        locked = TRUE
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #
