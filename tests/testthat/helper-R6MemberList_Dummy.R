source(file.path(testthat::test_path(), "helper-R6Member_Dummy.R"))
# ========================== > R6MemberList_Dummy < ========================== #

R6MemberList_Dummy <-
    R6MemberList$new(
        R6Member_Dummy,
        R6Member_Dummy2
    )

# ────────────────────────────────── <end> ─────────────────────────────────── #

R6Member_Dummy_new_val <-
    c(-8, 13)

R6Member_Dummy2_new_val <-
    c(1, 2, 3, -1, 0)
