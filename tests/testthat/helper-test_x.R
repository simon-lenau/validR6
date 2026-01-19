# ============================= > .test_getter < ============================= #

.test_getter <-
    function(
        Obj,
        field,
        expected_value,
        field_is_character = FALSE,
        env = TRUE
    ) {
        if (!field_is_character) {
            field <-
                deparse(substitute(field))
        }
        A <-
            Obj[[field]]
        B <-
            expected_value
        if (!env) {
            environment(A) <-
                environment(B) <-
                emptyenv()
        }
        test_that(.make_label(field, "correct value"), {
            expect_equal(
                A,
                B
            )
        })
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================== > .test_setter_error < ========================== #

.test_setter_error <-
    function(Obj, field, value, field_is_character = FALSE) {
        if (!field_is_character) {
            field <-
                deparse(substitute(field))
        }
        test_that(
            .make_label(paste0(field, "<-", .capture(value))),
            {
                expect_error(
                    Obj[[field]] <-
                        value
                )
            }
        )
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================= > .test_setter_success < ========================= #

.test_setter_success <-
    function(Obj, field, value, field_is_character = FALSE) {
        if (!field_is_character) {
            field <-
                deparse(substitute(field))
        }
            test_that(
                .make_label(paste0(field, "<-", .capture(value))),
                {
                    orig_field_value <-
                        Obj[[field]]
                    expect_no_error(
                        Obj[[field]] <-
                            value
                    )
                    expect_equal(Obj[[field]], value)

                    Obj[[field]] <-
                        orig_field_value
                }
            )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ====================== > .test_accessor_get_success < ====================== #

.test_accessor_get_success <-
    function(Obj, field, expected_value, field_is_character = FALSE) {
        if (!field_is_character) {
            field <-
                deparse(substitute(field))
        }
        test_that(.make_label(field), {
            expect_equal(
                Obj$accessor(field_name = field),
                expected_value
            )
        })
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #
# ====================== > .test_accessor_set_success < ====================== #

.test_accessor_set_success <-
    function(Obj, field, value, field_is_character = FALSE) {
        if (!field_is_character) {
            field <-
                deparse(substitute(field))
        }
        test_that(
            .make_label(field),
            {
                orig_field_value <-
                    Obj[[field]]
                # Set new field value
                expect_no_error(Obj$accessor(
                    field_value = value,
                    field_name = field
                ))
                expect_equal(Obj[[field]], value)
                # Reset field value
                Obj[[field]] <-
                    orig_field_value
            }
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================= > .test_equal < ============================== #

.test_equal <-
    function(A, B, env = TRUE, label = NULL) {
        test_that(
            .make_label(label),
            {
                if (!env) {
                    environment(A) <-
                        environment(B) <-
                            emptyenv()
                }

                expect_equal(
                    A,
                    B
                )
            }
        )
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #
