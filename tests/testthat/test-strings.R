test_that("string slices", {
  skip_if_not(rn("stringi"))

  x <- stringi::stri_rand_lipsum(1)
  len <- 77L

  # By length
  res <- str_slice(x, n = len)
  expect_equal(collapse0(res, sep = ""), x, info = "collapsed res")
  lengths <- vapply(res, nchar, integer(1))
  expect_true(all(lengths <= len), info = "All lines within desired length")

  # By length and word
  res <- str_slice_by_word(x, n = len)
  expect_equal(
    collapse0(res, sep = " "),
    x,
    info = "By word:  Collapsed res"
  )
  lengths <- vapply(res, nchar, integer(1))
  expect_true(
    all(lengths <= len),
    info = "By word:  All lines within desired length"
  )

  xx <- c(x, x)
  expect_error(str_slice_by_word(xx, 80), class = "simpleError")
})

# Better outputs
expect_my_date <- function(res, exp_char, ...) {
  testthat::expect_equal(
    str_extract_date(res, ...),
    as.Date(exp_char),
    label = as.character(res),
    expected.label = exp_char
  )
}

expect_my_datetime <- function(res, exp_char, ...) {
  testthat::expect_equal(
    str_extract_datetime(res, ...),
    capply(exp_char, strptime, format = "%Y-%m-%d %H%M%S", tz = ""),
    label = as.character(res),
    expected.label = exp_char
  )
}

test_that("Extract dates", {
  expect_my_date("This is a file name 2020-02-21.csv", "2020-02-21")
  expect_my_date(
    c(
      "This is a file name 2020-02-21.csv",
      "No date",
      "2014-09-15 is a good date"
    ),
    c("2020-02-21", NA, "2014-09-15")
  )
  expect_my_date(
    "Last saved 17 December 2019", "2019-12-17",
    format = "%d %B %Y"
  )

  expect_my_datetime(
    c("file date ending 2020-05-09 121212.xlsasdf",
      "1960-04-07 233044 is the time",
      "aaa 1984-12-14 001000"),
    c("2020-05-09 121212", "1960-04-07 233044", "1984-12-14 001000")
  )
})

test_that("print_c()", {
  expect_equal(
    utils::capture.output(print_c(1:3)),
    c("c(", "1,", "2,", "3,", "NULL", ")")
  )

  expect_equal(
    utils::capture.output(print_c(c("b", "a", "a", "b"))),
    c("c(", '\"a\",', '\"b\",', "NULL", ")")
  )

  expect_equal(
    utils::capture.output(print_c("a", null = FALSE)),
    c("c(", '\"a\"', ")")
  )
})
