context("String utility testing")

test_that("str-close-enough example is correct.", {
  res1 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = F)
  res2 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = T)

  expect_equal(res1, c(TRUE, TRUE, FALSE))
  expect_equal(res2, c(FALSE, FALSE, TRUE))
})

test_that("string slices", {
  x <- stringi::stri_rand_lipsum(1)
  len <- 77L

  # By length
  res <- str_slice(x, n = len)
  expect_equal(paste(res, collapse = ""), x, info = "collapsed res")
  lengths <- vapply(res, nchar, integer(1))
  expect_true(all(lengths <= len), info = "All lines within desired length")

  # By length and word
  res <- str_slice_by_word(x, n = len)
  expect_equal(paste(res, collapse = ""), x, info = "By word:  Collapsed res")
  lengths <- vapply(res, nchar, integer(1))
  expect_true(all(lengths <= len), info = "By word:  All lines within desired length")

  xx <- c(x, x)
  expect_error(str_slice_by_word(xx, 80), info = "Must take character(1)")
})

# Better outputs
expect_my_date <- function(res, exp_char, ...) {
  expect_equal(
    str_extract_date(res, ...),
    as.Date(exp_char),
    label = as.character(res),
    expected.label = exp_char
  )
}

test_that("Extract dates", {
  expect_my_date("This is a file name 2020-02-21.csv", "2020-02-21")
  expect_my_date(
    c("This is a file name 2020-02-21.csv", "No date"),
    c("2020-02-21", NA)
  )
  expect_my_date("Last saved 17 December 2019", "2019-12-17", format = "%d %B %y")
})
