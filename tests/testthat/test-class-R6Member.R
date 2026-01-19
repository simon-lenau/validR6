# =============================== > Getters < ================================ #

options(
  test_section = c("R6Member", "Getters")
)

.test_getter(R6Member_Dummy, name, "R6Member_Dummy")
.test_getter(R6Member_Dummy, private_name, ".R6Member_Dummy")
.test_getter(R6Member_Dummy, value, c(1, 4))
.test_getter(R6Member_Dummy, restrictions, restriction_list)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Setters < ================================ #

# ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
# ││ Test setter functions                                                  ││ #
# └└────────────────────────────────────────────────────────────────────────┘┘ #
options(
  test_section = c("R6Member", "Setters")
)

# locked / value_locked: only single logical values
.test_setter_success(R6Member_Dummy, locked, TRUE)
.test_setter_success(R6Member_Dummy, locked, FALSE)
.test_setter_error(R6Member_Dummy, locked, c(TRUE, FALSE))
.test_setter_error(R6Member_Dummy, locked, NULL)

.test_setter_success(R6Member_Dummy, value_locked, TRUE)
.test_setter_success(R6Member_Dummy, value_locked, FALSE)
.test_setter_error(R6Member_Dummy, value_locked, c(TRUE, FALSE))
.test_setter_error(R6Member_Dummy, value_locked, NULL)

# Name: accepts only string without whitespaces
.test_setter_error(R6Member_Dummy, name, "y x")
.test_setter_error(R6Member_Dummy, name, NULL)
.test_setter_success(R6Member_Dummy, name, "R6Member_Dummy_v2")

# private_name: setting not possible
.test_setter_error(R6Member_Dummy, private_name, " ")
.test_setter_error(R6Member_Dummy, private_name, NULL)
.test_setter_error(R6Member_Dummy, private_name, "New_Name")

# Value: Checks restrictions (length == 2 and sum == 5)
.test_setter_success(R6Member_Dummy, value, 5)
.test_setter_success(R6Member_Dummy, value, c(2, 3))
.test_setter_error(R6Member_Dummy, value, c(1, 1, 3))
.test_setter_error(R6Member_Dummy, value, c(1, 5))
R6Member_Dummy$restrictions$length$restriction

# Restrictions:
# - Accepts only R6MemberRestrictions
# - Checks validity of current value

.test_setter_error(
  R6Member_Dummy,
  restrictions,
  c(restriction_list, list(5))
)

.test_setter_error(
  R6Member_Dummy,
  restrictions,
  list(function(x) x + 1)
)

.test_setter_success(
  R6Member_Dummy,
  restrictions,
  c(restriction_list, list(R6MemberRestriction_Diff))
)

.test_setter_error(
  R6Member_Dummy,
  restrictions,
  c(restriction_list, list(diff = R6MemberRestriction_Diff_invalid))
)

.test_setter_success(
  R6Member_Dummy,
  restrictions,
  c(restriction_list, restriction_list)
)


# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Methods < ================================ #
options(
  test_section = c("R6Member", "methods")
)
# ============================== > initialize < ============================== #

test_that(
  .make_label(
    "`initialize`",
    "correct object class"
  ),
  {
    expect_equal(sort(class(R6Member_Dummy)), sort(c("R6Member", "R6")))
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================= > `<-` < ================================= #

test_that(.make_label("`<-`", "returns reference"), {
  expect_no_error(
    R6Member_ref <-
      R6Member_Dummy
  )
  expect_no_error(
    R6Member_ref$name <-
      "Cloned"
  )
  expect_equal(R6Member_Dummy$name, "Cloned")
  R6Member_Dummy$name <-
    R6Member_Dummy_original$name
})
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > clone < ================================= #

test_that(
  .make_label("`clone`", "returns new object"),
  {
    expect_no_error(
      R6Member_clone <-
        R6Member_Dummy$clone()
    )

    R6Member_clone$name <-
      "Cloned"

    expect_equal(R6Member_Dummy$name, "R6Member_Dummy")
    expect_equal(R6Member_clone$name, "Cloned")
    expect_mapequal(R6Member_clone$restrictions, R6Member_Dummy$restrictions)
  }
)
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ======================== > declare_active_binding < ======================== #
options(
  test_section = c(getOption("test_section"), "declare_active_binding")
)
test_that(
  .make_label("returns list with function"),
  {
    R6Member_Dummy_binding <-
      R6Member_Dummy$declare_active_binding()

    expect_type(R6Member_Dummy_binding, "list")
    expect_equal(class(R6Member_Dummy_binding[[1]]), "function")
  }
)

test_that(
  .make_label("has name"),
  {
    R6Member_Dummy_binding <-
      R6Member_Dummy$declare_active_binding()

    expect_equal(names(R6Member_Dummy_binding), R6Member_Dummy$name)
  }
)

test_that(
  .make_label("contains accessor"),
  {
    R6Member_Dummy_binding <-
      R6Member_Dummy$declare_active_binding()[[1]]

    expect_equal(
      as.character(body(R6Member_Dummy_binding))[[1]],
      "private$.R6Member_Dummy$accessor"
    )
  }
)


# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > accessor < =============================== #

# =============================== > getters < ================================ #

options(
  test_section = c(
    "R6Member",
    "methods",
    "accessor",
    "dispatches to getters"
  )
)

.test_accessor_get_success(R6Member_Dummy, name, "R6Member_Dummy")
.test_accessor_get_success(R6Member_Dummy, value, c(1, 4))
.test_accessor_get_success(R6Member_Dummy, restrictions, restriction_list)
.test_accessor_get_success(R6Member_Dummy, locked, FALSE)
.test_accessor_get_success(R6Member_Dummy, value_locked, FALSE)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > setters < ================================ #

options(
  test_section = c(
    "R6Member",
    "methods",
    "accessor",
    "dispatches to setters"
  )
)

.test_accessor_set_success(R6Member_Dummy, name, "SOME_NEW_NAME")
.test_accessor_set_success(R6Member_Dummy, value, c(-3, 8))
.test_accessor_set_success(
  R6Member_Dummy,
  restrictions,
  c(
    restriction_list,
    list(diff = R6MemberRestriction_Diff)
  )
)
.test_accessor_set_success(R6Member_Dummy, locked, TRUE)
.test_accessor_set_success(R6Member_Dummy, value_locked, TRUE)

# ────────────────────────────────── <end> ─────────────────────────────────── #
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ======================== > declare_initialization < ======================== #
options(
  test_section = c(
    "R6Member",
    "methods",
    "declare_initialization"
  )
)
test_that(
  .make_label("returns expression"),
  {
    expect_type(R6Member_Dummy$declare_initialization(), "expression")
  }
)

test_that(
  .make_label("is expression"),
  {
    expect_equal(
      as.character(R6Member_Dummy$declare_initialization()),
      "self$R6Member_Dummy <- R6Member_Dummy"
    )
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > print < ================================= #

options(
  test_section = c(
    "R6Member",
    "methods",
    "print"
  )
)
test_that(
  .make_label("has correct formals"),
  {
    expect_named(
      formals(R6Member_Dummy$print),
      c('value_level', 'restriction_level', 'print_class')
    )
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Locking < ================================ #
options(
  test_section = c(
    "R6Member",
    "methods",
    "locking"
  )
)

test_that(
  .make_label("(Un-)Locking also (un-)locks value"),
  {
    R6Member_Dummy$locked <-
      TRUE
    expect_equal(R6Member_Dummy$value_locked, TRUE)
    R6Member_Dummy$locked <-
      FALSE
    expect_equal(R6Member_Dummy$value_locked, FALSE)
  }
)

test_that(
  .make_label("Locking prevents all setters"),
  {
    R6Member_Dummy_clone <-
      R6Member_Dummy$clone()
    R6Member_Dummy_clone$locked <-
      TRUE

    # Setter for `name`
    expect_error({
      R6Member_Dummy_clone$name <-
        "New_Name"
    })
    # Setter for `value`
    expect_error({
      R6Member_Dummy_clone$value <-
        5
    })
    # Setter for `restrictions`
    expect_error({
      R6Member_Dummy_clone$restrictions <-
        NULL
    })
  }
)

test_that(
  .make_label("Unlocking allows all setters"),
  {
    R6Member_Dummy_clone <-
      R6Member_Dummy$clone()
    R6Member_Dummy_clone$locked <-
      FALSE

    # Setter for `name`
    expect_no_error({
      R6Member_Dummy_clone$name <-
        "New_Name"
    })
    # Setter for `value`
    expect_no_error({
      R6Member_Dummy_clone$value <-
        5
    })
    # Setter for `restrictions`
    expect_no_error({
      R6Member_Dummy_clone$restrictions <-
        NULL
    })

    expect_equal(R6Member_Dummy_clone$name, "New_Name")
    expect_equal(R6Member_Dummy_clone$value, 5)
    expect_equal(R6Member_Dummy_clone$restrictions, NULL)
  }
)

test_that(
  .make_label("Value can be unlocked independently"),
  {
    R6Member_Dummy_clone <-
      R6Member_Dummy$clone()
    R6Member_Dummy_clone$locked <-
      TRUE

    R6Member_Dummy_clone$value_locked <-
      FALSE

    # Setter for `name`
    expect_error({
      R6Member_Dummy_clone$name <-
        "New_Name"
    })
    # Setter for `value`
    expect_no_error({
      R6Member_Dummy_clone$value <-
        5
    })
    # Setter for `restrictions`
    expect_error({
      R6Member_Dummy_clone$restrictions <-
        NULL
    })
  }
)

test_that(
  .make_label("Value can be locked independently"),
  {
    R6Member_Dummy_clone <-
      R6Member_Dummy$clone()
    R6Member_Dummy_clone$locked <-
      FALSE

    R6Member_Dummy_clone$value_locked <-
      TRUE

    # Setter for `name`
    expect_no_error({
      R6Member_Dummy_clone$name <-
        "New_Name"
    })
    # Setter for `value`
    expect_error({
      R6Member_Dummy_clone$value <-
        5
    })
    # Setter for `restrictions`
    expect_no_error({
      R6Member_Dummy_clone$restrictions <-
        NULL
    })
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #
