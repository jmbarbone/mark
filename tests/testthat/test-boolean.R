
# to_boolean.logical() ----------------------------------------------------

test_that("to_boolean.logical() works", {
  x <- c(TRUE, TRUE, FALSE, FALSE, NA)
  expect_identical(to_boolean(x), x)
})

# to_boolean.numeric() ----------------------------------------------------

test_that("to_boolean.numeric() works", {
  expect_identical(
    to_boolean(c(1, 2, -1, 0, NA, 3)),
    c(TRUE, NA, NA, FALSE, NA, NA)
  )
})


# to_boolean.character() --------------------------------------------------

wtf <- c("what", "true", "T", "TRUE", "false", "F", "false")
yn <- c("Y", "yes", "N", "no")

test_that("to_boolean.character() works", {
  expect_identical(
    to_boolean(wtf),
    c(NA, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    to_boolean(yn),
    c(TRUE, TRUE, FALSE, FALSE)
  )

  expect_error(to_boolean("a", 1, 2), "Not yet finished")
})


# to_boolean.factor() -----------------------------------------------------

test_that("to_boolean.factor() works", {
  expect_identical(
    to_boolean(fact(wtf)),
    c(NA, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    to_boolean(fact(yn)),
    c(TRUE, TRUE, FALSE, FALSE)
  )

  expect_error(to_boolean(fact("a"), 1, 2), "Not yet finished")
})


# is_true(), is_false() ---------------------------------------------------

test_that("is_true(), is_false() works", {
  x <- c(2, 3, NA)
  exp <- c(NA, NA, NA)

  res <- is_true(x)
  expect_identical(exp, res)

  res <- is_false(x)
  expect_identical(res, res)

  x <- c(TRUE, FALSE, NA)

  res <- is_true(x)
  exp <- c(TRUE, FALSE, FALSE)
  expect_identical(res, exp)

  res <- is_false(x)
  exp <- c(FALSE, TRUE, FALSE)
  expect_identical(res, exp)
})


# %xor% -------------------------------------------------------------------

test_that("%xor% works", {
  x <- TRUE
  y <- FALSE

  expect_identical(xor(x, y), x %xor% y)
})


# either() ----------------------------------------------------------------

test_that("either() works", {
  x <- c(TRUE, NA, FALSE, FALSE)
  y <- c(TRUE, TRUE, FALSE, NA)

  res <- either(x, y)
  exp <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(res, exp)
})


# none() ------------------------------------------------------------------

test_that("none() works", {
  x <- c(FALSE, FALSE, NA)
  expect_identical(none(x), NA)
  expect_true(none(x, na.rm = TRUE))

  x <- c(TRUE, NA)
  expect_false(none(x))
  expect_false(none(x, na.rm = TRUE))
})
