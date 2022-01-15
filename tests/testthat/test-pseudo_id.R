test_that("pseudo_id.default() works", {
  x <- c(1L, 1L, 4L, NA_integer_, 2L)
  res <- pseudo_id(x)

  expect_identical(
    res,
    struct(
      c(1L, 1L, 2L, 4L, 3L),
      uniques = c(1L, 4L, 2L, NA_integer_),
      class = c("pseudo_id", "integer"))
  )

  x <- c(1.2, 1.2, 4.999, NA, -2)
  res <- pseudo_id(x)

  expect_identical(
    res,
    struct(
      c(1L, 1L, 2L, 4L, 3L),
      uniques = c(1.2, 4.999, -2, NA_real_),
      class = c("pseudo_id", "integer"))
  )
})

test_that("pseudo_id.factor() works", {
  x <- struct(1:3, levels = letters[1:3], class = "factor")
  res <- pseudo_id(x)
  exp <- struct(1:3, uniques = letters[1:3], class = c("pseudo_id", "integer"))
  expect_identical(res, exp)
})

test_that("pseudo_id.factor() does not return NA values [#28]", {
  # NA in levels and values
  x <- struct(c(1:3, NA), levels = c(letters[1:3], NA), class = "factor")
  res <- pseudo_id(x)
  exp <- struct(1:4, uniques = c(letters[1:3], NA_character_), class = c("pseudo_id", "integer"))
  expect_identical(res, exp)

  # NA in values but not in levels
  x <- struct(c(1:3, NA), levels = letters[1:3], class = "factor")
  res <- pseudo_id(x)
  exp <- struct(1:4, uniques = c(letters[1:3], NA_character_), class = c("pseudo_id", "integer"))
  expect_identical(res, exp)
})

test_that("pseudo_id.pseudo_id() works", {
  x <- runif(10)
  x <- round(x, 2)
  id <- pseudo_id(x)
  expect_equal(pseudo_id(id), id)
})

test_that("snapshots", {
  x <- c(
    0.316596544580534,  0.838568194536492,   0.126674774102867,
    0.657560844207183,  0.636712728533894,   0.571150240721181,
    0.0811728567350656, 0.765485385898501,   0.562053531175479,
    0.807950841495767,  0.00996402697637677, 0.825092894025147
  )
  expect_snapshot(pseudo_id(x))
})
