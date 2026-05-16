
test_that("to_boolean() works", {
  x <- c(TRUE, FALSE, NA)
  expect_identical(to_boolean(x), x)

  x <- c(1, 0, 2, -1, NA)
  exp <- c(TRUE, FALSE, NA, NA, NA)
  expect_identical(to_boolean(x), exp)
  expect_identical(to_boolean(as.integer(x)), exp)

  x <- c(
    "true", "false",
    "TRUE", "FALSE",
    "T", "F",
    "yes", "no",
    "Y", "N",
    "what", "okay",
    NA
  )

  exp <- c(
    TRUE, FALSE,
    TRUE, FALSE,
    TRUE, FALSE,
    TRUE, FALSE,
    TRUE, FALSE,
    NA, NA,
    NA
  )

  expect_identical(to_boolean(x), exp)
  expect_identical(to_boolean(factor(x)), exp)
})

test_that("to_boolean() errors", {
  expect_error(
    to_boolean(0, true = 1, false = 1),
    class = "bad_bools_input"
  )

  expect_error(
    to_boolean(1:4, 1, 2, 3),
    class = "bad_bools_unmatched"
  )
})
