# ================================ > Fields < ================================ #

options(
    test_section = c("R6Class Instance", "Fields")
)


# ============================ > Public fields < ============================= #

test_that(
    .make_label("Public"),
    {
        expect_equal(
            intersect(
                SomeR6ClassDefinitionList$names(
                    type = "public",
                    subset = "public"
                ),
                names(SomeR6Class_instance)
            ),
            SomeR6ClassDefinitionList$names(type = "public", subset = "public")
        )
    }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > Private fields < ============================ #

test_that(
    .make_label("Private"),
    {
        expect_equal(
            intersect(
                SomeR6ClassDefinitionList$names(
                    type = "private",
                    subset = "all"
                ),
                names(SomeR6Class_instance$`.__enclos_env__`$private)
            ),
            SomeR6ClassDefinitionList$names(type = "private", subset = "all")
        )
    }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > getters < ================================ #
options(
    test_section = c("R6Class Instance", "Fields", "getters")
)

for (field_name in setdiff(
    SomeR6ClassDefinitionList$names(
        type = "public",
        subset = "public"
    ),
    names(SomeR6Class$public_methods)
)) {
    .test_getter(
        SomeR6Class_instance,
        field_name,
        SomeR6ClassDefinitionList$select(field_name)$value,
        field_is_character = TRUE,
        env = !is.function(SomeR6ClassDefinitionList$select(field_name)$value)
    )
}
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > setters < ================================ #

# ======================== > work for public fields < ======================== #

options(
    test_section = c(
        "R6Class Instance",
        "Fields",
        "accessors",
        "setters",
        "work for public fields"
    )
)

.test_setter_success(
    SomeR6Class_instance,
    attr_list,
    list(u = 1:3)
)

.test_setter_success(
    SomeR6Class_instance,
    attr_int,
    value = (1:3) * 3
)


.test_setter_success(
    SomeR6Class_instance,
    fun_list_elem,
    value = function(x) x + 1
)


# ────────────────────────────────── <end> ─────────────────────────────────── #
# ======================= > fail for private fields < ======================== #

options(
    test_section = c(
        "R6Class Instance",
        "Fields",
        "accessors",
        "setters",
        "fail for private fields"
    )
)


.test_setter_error(
    SomeR6Class_instance,
    private_attr_num,
    1:3
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > fail for methods < =========================== #

options(
    test_section = c(
        "R6Class Instance",
        "Fields",
        "accessors",
        "setters",
        "fail for methods"
    )
)


.test_setter_error(
    SomeR6Class_instance,
    mtd_multiply,
    function(x) x + 1
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ===================== > fail on invalid restrictions < ===================== #

options(
    test_section = c(
        "R6Class Instance",
        "Fields",
        "accessors",
        "setters",
        "fail on invalid restrictions"
    )
)

for (field_name in setdiff(
    SomeR6ClassDefinitionList$names(
        type = "public",
        subset = "public"
    ),
    names(SomeR6Class$public_methods)
)) {
    .test_setter_error(
        SomeR6Class_instance,
        field_name,
        "invalid assignment",
        field_is_character = TRUE
    )
}
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ────────────────────────────────── <end> ─────────────────────────────────── #
