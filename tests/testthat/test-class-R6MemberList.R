# =============================== > Methods < ================================ #
options(
    test_section = c("R6MemberList", "Methods")
)

# ============================== > initialize < ============================== #
options(
    test_section = c("R6MemberList", "Methods", "initialize")
)

test_that(
    .make_label(
        "correct object class"
    ),
    {
        expect_equal(
            sort(class(R6MemberList_Dummy)),
            sort(c("R6MemberList", "R6"))
        )
    }
)
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > active_bindings < ============================ #
options(
    test_section = c("R6MemberList", "Methods", "active_bindings")
)
test_that(
    .make_label(
        "Correct names"
    ),
    {
        expect_named(
            R6MemberList_Dummy$active_bindings(),
            c("R6Member_Dummy", "R6Member_Dummy2")
        )
    }
)

test_that(
    .make_label(
        "are functions"
    ),
    {
        expect_equal(
            all(
                Reduce(
                    c,
                    lapply(R6MemberList_Dummy$active_bindings(), class)
                ) %in%
                    "function"
            ),
            TRUE
        )
    }
)


test_that(
    .make_label(
        "have single argument"
    ),
    {
        expect_equal(
            all(
                Reduce(
                    c,
                    lapply(
                        R6MemberList_Dummy$active_bindings(),
                        function(x) length(formals(x))
                    )
                ) %in%
                    1
            ),
            TRUE
        )
    }
)

test_that(
    .make_label(
        "use accessor"
    ),
    {
        active_bindings <-
            R6MemberList_Dummy$active_bindings()

        expect_equal(
            all(
                Reduce(
                    c,
                    lapply(
                        names(active_bindings),
                        function(i) {
                            grepl(
                                paste0("^private\\$\\.", i, "\\$accessor"),
                                as.character(body(active_bindings[[i]]))[1],
                                perl = TRUE
                            )
                        }
                    )
                ) %in%
                    TRUE
            ),
            TRUE
        )
    }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================= > public_list < ============================== #
options(
    test_section = c("R6MemberList", "Methods", "public_list")
)
test_that(
    .make_label(
        "provides initialize & (optional) print method"
    ),
    {
        expect_named(
            R6MemberList_Dummy$public_list(print_method = "none"),
            c("initialize")
        )
        expect_named(
            R6MemberList_Dummy$public_list(print_method = "default"),
            c("print", "initialize"),
            ignore.order = TRUE
        )
    }
)
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > private_list < ============================= #
options(
    test_section = c("R6MemberList", "Methods", "private_list")
)

test_that(
    .make_label(
        "provides list of members"
    ),
    {
        expect_named(
            R6MemberList_Dummy$private_list()$`__R6Members__`,
            c("public", "private"),
            ignore.order = TRUE
        )

        expect_equal(
            R6MemberList_Dummy$private_list()$`__R6Members__`$public,
            c("R6Member_Dummy", "R6Member_Dummy2")
        )
    }
)

test_that(
    .make_label(
        "Contains R6Members"
    ),
    {
        private_list <-
            R6MemberList_Dummy$private_list()

        expect_equal(
            all(
                paste0(".", c("R6Member_Dummy", "R6Member_Dummy2")) %in%
                    names(private_list)
            ),
            TRUE
        )
        lapply(
            paste0(".", c("R6Member_Dummy", "R6Member_Dummy2")),
            function(x) {
                expect_equal(
                    "R6Member" %in% class(private_list[[x]]),
                    TRUE
                )
            }
        )
    }
)

test_that(
    .make_label(
        "method for printing members with correct arguments"
    ),
    {
        private_list <-
            R6MemberList_Dummy$private_list()

        expect_equal(
            all(
                ".print" %in%
                    names(private_list)
            ),
            TRUE
        )

        expect_equal(
            is.function(private_list$.print),
            TRUE
        )
        names(formals(private_list$.print))
        expect_named(
            formals(private_list$.print),
            c(
                "value_level",
                "restriction_level",
                "print_class",
                "prefix",
                "suffix"
            ),
            ignore.order = TRUE
        )
    }
)


# ────────────────────────────────── <end> ─────────────────────────────────── #
# ================================ > names < ================================= #
options(
    test_section = c("R6MemberList", "Methods", "names")
)
test_that(
    .make_label(
        "Returns public names"
    ),
    {
        expect_equal(
            R6MemberList_Dummy$names(),
            c("R6Member_Dummy", "R6Member_Dummy2")
        )
    }
)
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================== > S3-Methods < ============================== #
# ================================= > `[` < ================================== #

options(
    test_section = c("R6MemberList", "Methods", "S3", "`[`")
)
test_that(
    .make_label(),
    {
        expect_equal(
            R6MemberList_Dummy[c("R6Member_Dummy2", "R6Member_Dummy")],
            R6MemberList$new(R6Member_Dummy2, R6Member_Dummy)
        )
    }
)


# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > `[<-` < ================================= #

options(
    test_section = c("R6MemberList", "Methods", "S3", "`[<-`")
)


test_that(
    .make_label("works"),
    {
        expect_no_error(
            R6MemberList_Dummy[c("R6Member_Dummy2", "R6Member_Dummy")] <-
                list(R6Member_Dummy2_new_val, R6Member_Dummy_new_val)
        )

        expect_equal(
            R6MemberList_Dummy$select("R6Member_Dummy")$value,
            R6Member_Dummy_new_val
        )

        expect_equal(
            R6MemberList_Dummy$select("R6Member_Dummy2")$value,
            R6Member_Dummy2_new_val
        )
    }
)

test_that(
    .make_label("checks constraints"),
    {
        expect_error(
            R6MemberList_Dummy[c(
                "R6Member_Dummy2",
                "R6Member_Dummy"
            )] <-
                list(R6Member_Dummy2_new_val, R6Member_Dummy_new_val - 0.1)
        )

        expect_error(
            R6MemberList_Dummy[c(
                "R6Member_Dummy2",
                "R6Member_Dummy"
            )] <-
                list(R6Member_Dummy2_new_val + 0.1, R6Member_Dummy_new_val)
        )
    }
)


# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================= > `[[` < ================================= #

options(
    test_section = c("R6MemberList", "Methods", "S3", "`[[`")
)
test_that(
    .make_label(),
    {
        expect_equal(
            R6MemberList_Dummy[["R6Member_Dummy"]],
            R6MemberList_Dummy$select("R6Member_Dummy")
        )
    }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > `[[<-` < ================================ #

options(
    test_section = c("R6MemberList", "Methods", "S3", "`[[<-`")
)
test_that(
    .make_label("works"),
    {
        expect_no_error(
            R6MemberList_Dummy[["R6Member_Dummy"]] <-
                R6Member_Dummy_new_val
        )

        expect_equal(
            R6MemberList_Dummy$select("R6Member_Dummy")$value,
            R6Member_Dummy_new_val
        )

        expect_no_error(
            R6MemberList_Dummy[["R6Member_Dummy2"]] <-
                R6Member_Dummy2_new_val
        )

        expect_equal(
            R6MemberList_Dummy$select("R6Member_Dummy2")$value,
            R6Member_Dummy2_new_val
        )
    }
)

test_that(
    .make_label("checks constraints"),
    {
        expect_error(
            R6MemberList_Dummy[["R6Member_Dummy"]] <-
                R6Member_Dummy2_new_val + 0.1
        )
        expect_error(
            R6MemberList_Dummy[["R6Member_Dummy"]] <-
                R6Member_Dummy_new_val + 0.1
        )
    }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ────────────────────────────────── <end> ─────────────────────────────────── #
