test_that("dates - utils", {
  expect_named(days_in_month)

  expect_true(is_valid_date_string("2019"))
  expect_true(is_valid_date_string("Jan 19 2020"))
  # too difficult to determine year for max?
  expect_true(is_valid_date_string("01 Feb"))

  expect_false(is_valid_date_string(""))
  expect_false(is_valid_date_string("..."))
  expect_false(is_valid_date_string("  "))
  expect_false(is_valid_date_string("fdsafs"))
  expect_false(is_valid_date_string("UNK"))
  expect_false(is_valid_date_string(NA_character_))
})


test_that("leap years", {
  expect_false(is_leap_year(1500))
  expect_true( is_leap_year(1600))
  expect_false(is_leap_year(1700))
  expect_false(is_leap_year(1800))
  expect_false(is_leap_year(1900))
  expect_true( is_leap_year(2000))
  expect_true( is_leap_year(2400))
  expect_true( is_leap_year(4000))
  expect_true( is_leap_year(0))
  expect_false(is_leap_year(100))
  expect_false(is_leap_year(102))
  expect_true( is_leap_year(400))
  expect_true( is_leap_year(as.POSIXct("2020-01-01")))
})

# # Waldo prints out dates as days from origin...
# waldo::compare(as.Date("2020-12-18"), as.Date("2020-12-17"))
# waldo::compare("2020-12-18", "2020-12-17")
foo <- function(x, ...) {
  as.character(date_from_partial(x, ...))
}

test_that("Some examples", {
  x <- "UN UNK 2019"
  expect_equal(foo(x, format = "dmy"), "2019-01-01")
  expect_equal(foo(x, format = "dmy", method = "max"), "2019-12-31")

  expect_equal(
    foo(c("01 JAN 1996", "Feb 2010", "2019"), format = "dmy"),
    c("1996-01-01", "2010-02-01", "2019-01-01")
  )

  expect_equal(
    foo(c("01 JAN 1996", "Feb 2016"), "dmy", method = "max"),
    c("1996-01-01", "2016-02-29")
  )

  expect_equal(foo("2015", method = "min"), "2015-01-01")
  expect_equal(foo("2015", method = "max"), "2015-12-31")
  expect_equal(foo("2015", format = "dmy", method = "min"), "2015-01-01")
  expect_equal(foo("2015", format = "dmy", method = "max"), "2015-12-31")
})

test_that("Bad date: Earliest", {
  dates <- c("3 UNK 2019", "UN JUN 2004", "Feb 2000",   "UK UNK UNKN")
  exp <-   c("2019-01-03", "2004-06-01",  "2000-02-01", NA_character_)
  expect_equal(foo(dates, format = "dmy"), exp)
})

test_that("Bad date: Latest", {
  dates <- c("3 UNK 2019", "UN JUN 2004", "Feb 2000",   "Feb 2100",   "UK UNK UNKN")
  exp <-  c("2019-12-03",  "2004-06-30",  "2000-02-29", "2100-02-28", NA_character_)
  expect_equal(foo(dates, format = "dmy", method = "max"), exp)
})

test_that("'Empty' dates don't cause errors", {
  expect_error(date_from_partial(""), NA)
  expect_error(date_from_partial("    "), NA)
  expect_error(date_from_partial("."), NA)
  expect_error(date_from_partial("?.."), NA)
  expect_s3_class(date_from_partial(NA), "Date")
})

test_that("date errors", {
  expect_error(verify_format("ymda"))
  expect_error(verify_format("abc"))
  expect_error(verify_format("aaa"))
  expect_error(verify_format("ymd"), NA)
  expect_error(verify_format("dmy"), NA)
  expect_error(verify_format("mdy"), NA)
  res <- parse_date_strings("y-m-d-a", "ymd")
  expect_identical(res, NA_Date_)
})
