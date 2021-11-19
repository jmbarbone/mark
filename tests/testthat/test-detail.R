test_that("details() works", {
  x <- 1:3
  y <- factor(letters[1:3])
  z <- c("x", NA_character_, "z")
  attr(z, "label") <- "information"
  df <- data.frame(x = x, y = factor(letters[1:3]))
  expect_error(detail(x), NA)
  expect_error(detail(df), NA)

  expect_error(detail(data.frame()))

  exp <- quick_dfl(
    class   = "logical",
    type    = "logical",
    label   = NA_character_,
    n       = 0L,
    na      = 1L,
    min_c   = NA_character_,
    max_c   = NA_character_,
    level   = NA_character_,
    level_n = NA_integer_,
    note    = NA_character_,
    comment = NA_character_
  )
  # also no warnings
  expect_identical(detail(NA), exp)
})

test_that("details() keeps factors [50]", {
  expect_identical(detail(factor("a"))$class, "factor")
  expect_identical(detail(ordered("a"))$class, "ordered; factor")
})

test_that("details() and tibbles", {
  skip_if_not_installed("tibble")

  expect_error(detail(tibble::tibble(a = 1, b = 1:3)), NA)
  expect_error(detail(tibble::tibble(a = 1, b = list(1:3))), NA)
  expect_error(detail(tibble::tibble(a = NULL, b = list(1:3))))
})

test_that("details.data.frame() passes with single column [48]", {
  expect_error(data.frame(a = 1), NA)
})
