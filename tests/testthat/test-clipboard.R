need_clipr <- function() {
  testthat::skip_if_not_installed("clipr")

  # not sure if this is needed
  withr::with_envvar(c(CLIPR_ALLOW = TRUE), {
    testthat::skip_if_not(clipr::clipr_available())
  })

  testthat::skip_on_cran()
}

test_that("clipboard", {
  need_clipr()
  clear_clipboard()

  test_clipboard <- function(x, ...) {
    expect_error(write_clipboard(x), NA)
    expect_equal(read_clipboard(), x, ...)
  }

  test_clipboard(c(TRUE, FALSE, NA))
  test_clipboard(c(0.1234, -0.1586, 0.0001200))
  test_clipboard(-1:4)
  test_clipboard(as.Date("2020-01-02") + 0:4)
  test_clipboard(runif(1e4))

  expect_error(clear_clipboard(), NA)
  expect_equal(read_clipboard(), NULL) # previously ""

  x <- quick_dfl(
    var1 = 1:3,
    var2 = letters[1:3],
    var3 = as.Date("2020-01-03") + 1:3,
    var4 = c(TRUE, FALSE, NA)
  )

  expect_error(write_clipboard(x), NA)
  res <- read_clipboard("data.frame")
  expect_s3_class(res, "data.frame")
  if (package_available("tibble"))
  expect_equal(as.data.frame(res), x)

  # finally test tibble
  skip_if_not_installed("tibble")
  expect_s3_class(res, "tbl_df")
  expect_equal(read_clipboard("tibble"), tibble::as_tibble(x))
})

test_that("clipboard methods", {
  need_clipr()
  expect_clip <- function(input, method) {
    write_clipboard(input)
    res <- if (package_available("tibble")) {
      tibble::tibble(a = 1L, b = 2L, c = 3L)
    } else {
      fuj::quick_dfl(a = 1L, b = 2L, c = 3L)
    }
    expect_identical(read_clipboard(method), res)
  }

  simple_tbl <- function(delim) {
    paste(
      paste(letters[1:3], collapse = delim),
      paste(1:3, collapse = delim),
      sep = "\n"
    )
  }

  expect_clip(simple_tbl("\t"), "data.frame")
  expect_clip(simple_tbl("\t"), "excel")
  expect_clip(simple_tbl("\t"), "calc")
  expect_clip(simple_tbl("\t"), "tibble")
  expect_clip(simple_tbl(","), "csv")
  expect_clip(simple_tbl(";"), "csv2")
  expect_clip(simple_tbl(";"), "csv2")
  expect_clip(simple_tbl("|"), "bsv")
  expect_clip(simple_tbl("|"), "psv")
  expect_clip(simple_tbl("\t"), "tsv")
  skip_if_not_installed("readMDTable")
  expect_clip("| a | b | c |\n|--:|--:|--:|\n| 1 | 2 | 3 |", "md")
})

test_that("utils_type_convert()", {
  expect_equal(type_convert2(1L), 1L)

  xchr <- c("this", "that", NA, "121", "them", ".011", "2020", "NA")
  xdbl <- c("-0.10", "  .10123", "-.102010000  ", "NaN")
  xint <- c("121021", "-12191", "121001", "  ")
  xdat <- c("2020-05-01", "1900-10-10", "1655-06-07")
  xlgl <- c("TRUE", "  true", "FALSE", "fALSE", "na", "NA")
  rchr <- c("this", "that", NA, "121", "them", ".011", "2020", NA)
  rdbl <- as.double(xdbl)
  rint <- as.integer(xint)
  rdat <- as.Date(xdat)
  rlgl <- as.logical(toupper(xlgl))

  expect_equal(type_convert2(xchr), rchr)
  expect_equal(type_convert2(xdbl), rdbl)
  expect_equal(type_convert2(rdbl), rdbl)
  expect_equal(type_convert2(xint), rint)
  expect_equal(type_convert2(rint), rint)
  expect_equal(type_convert2(xdat), rdat)
  expect_equal(type_convert2(rdat), rdat)
  expect_equal(type_convert2(xlgl), rlgl)
  expect_equal(type_convert2(rlgl), rlgl)
})
