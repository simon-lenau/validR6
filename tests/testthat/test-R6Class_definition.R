# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================= > Constructor < ============================== #

options(
    test_section = c("R6Class definition", "Constructor")
)


test_that(
    .make_label("formals", "correct names"),
    {
        expect_named(
            formals(SomeR6Class$public_methods$initialize),
            SomeR6ClassDefinitionList$names(type = "public", subset = "public"),
            ignore.order = TRUE
        )
    }
)


for (i in SomeR6ClassDefinitionList$names(type = "public", subset = "public")) {
    .test_equal(
        SomeR6ClassDefinitionList$select(i)$value,
        eval(
            formals(SomeR6Class$public_methods$initialize)[[i]]
        ),
        env = FALSE,
        label = c("formals", "correct defaults", i)
    )
}

for (i in SomeR6ClassDefinitionList$names(type = "public", subset = "public")) {
    test_that(
        .make_label(c("formals", "assignments", i)),
        {
            expect_equal(
                sum(
                    grepl(
                        if (SomeR6ClassDefinitionList$select(i)$is_method) {
                            paste0(
                                "private\\$[^\\s]*?",
                                i,
                                "[^\\s]*?\\$value\\s*<-\\s*",
                                i
                            )
                        } else {
                            paste0("self\\$", i, "\\s*<-\\s*", i)
                        },
                        as.character(body(
                            SomeR6Class$public_methods$initialize
                        )),
                        perl = !TRUE
                    ),
                    na.rm = TRUE
                ) >
                    0,
                TRUE
            )
        }
    )
}
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================== > R6Members < =============================== #

# ============================ > initial values < ============================ #

options(
    test_section = c("R6Class definition", "R6Members", "initial values")
)

for (i in SomeR6ClassDefinitionList$names()) {
    private_name <-
        paste0(".", i)
    .test_equal(
        SomeR6Class$private_fields[[private_name]]$value,
        SomeR6ClassDefinitionList$select(i)$value,
        env = FALSE,
        label = c(i)
    )
}
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > restrictions < ============================= #

# ────────────────────────────────── <end> ─────────────────────────────────── #
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================== > x < =================================== #

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================== > x < =================================== #

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================== > x < =================================== #

# ────────────────────────────────── <end> ─────────────────────────────────── #
