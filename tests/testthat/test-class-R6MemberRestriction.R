# =============================== > Getters < ================================ #

options(
  test_section = c("R6MemberRestriction", "Getters")
)
.test_getter(R6MemberRestriction_Sum, id, "Sum")
.test_getter(R6MemberRestriction_Len, id, "Length")

.test_getter(
  R6MemberRestriction_Sum,
  restriction,
  function(x) sum(x) == 5,
  env = FALSE
)
.test_getter(
  R6MemberRestriction_Len,
  restriction,
  function(x) if (length(x) > 2) stop("Length should be <= 2!"),
  env = FALSE
)


# ────────────────────────────────── <end> ─────────────────────────────────── #
# =============================== > Setters < ================================ #
options(
  test_section = c("R6MemberRestriction", "Setters")
)
# Id: accepts only length-one values
.test_setter_error(R6MemberRestriction_Sum, id, c("A", "B"))
.test_setter_success(R6MemberRestriction_Sum, id, "A")
.test_setter_success(R6MemberRestriction_Sum, id, 3)

# Restriction: accepts only functions
.test_setter_error(R6MemberRestriction_Sum, restriction, "X")
.test_setter_error(R6MemberRestriction_Sum, restriction, list())
.test_setter_success(R6MemberRestriction_Sum, restriction, function(x) {
  sum(x) == 5
})

# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Methods < ================================ #
options(
  test_section = c("R6MemberRestriction", "methods")
)
# ================================ > check < ================================= #

test_that(
  .make_label(
    "`check`",
    "works for valid values"
  ),
  {
    expect_equal(
      R6MemberRestriction_Sum$check(rep(1, 5)),
      list(decision = TRUE, message = "")
    )
    expect_equal(
      R6MemberRestriction_Len$check(c(1, 4)),
      list(decision = TRUE, message = "")
    )
  }
)


test_that(
  .make_label(
    "`check`",
    "fails for invalid values"
  ),
  {
    expect_equal(
      R6MemberRestriction_Sum$check(rep(1.01, 25))$decision,
      FALSE
    )
    expect_equal(
      R6MemberRestriction_Len$check(letters)$decision,
      FALSE
    )
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================== > initialize < ============================== #

test_that(
  .make_label(
    "`initialize`",
    "correct object class"
  ),
  {
    expect_equal(
      sort(class(R6MemberRestriction_Sum)),
      sort(c("R6MemberRestriction", "R6"))
    )
  }
)

# ────────────────────────────────── <end> ─────────────────────────────────── #
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =============================== > Methods < ================================ #
# ────────────────────────────────── <end> ─────────────────────────────────── #
