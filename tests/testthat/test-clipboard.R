test_that("clipboard", {
  skip_if_not(interactive(), "Is not interactive")

  if (!is_windows()) {
    expect_error(write_clipboard())
    skip("Not windows")
  }

  skip_if(
    any(has_warning(integer(1e4), readClipboard)),
    "Failed to access clipboard"
  )

  clear_clipboard()

  test_clipboard <- function(x, ...) {
    expect_error(write_clipboard(x), NA)
    expect_equal(read_clipboard(), x, ...)
  }

  test_clipboard(c(TRUE, FALSE, NA))
  test_clipboard(c(0.1234, -0.1586, 0.0001200))
  test_clipboard(-1:4)
  test_clipboard(as.Date("2020-01-02") + 0:4)
  test_clipboard(runif(1e6))

  x <- quick_dfl(
    var1 = 1:3,
    var2 = letters[1:3],
    var3 = as.Date("2020-01-03") + 1:3,
    var4 = c(TRUE, FALSE, NA)
  )

  expect_error(write_clipboard(x), NA)
  expect_equal(read_clipboard("data.frame"), x)

  expect_error(clear_clipboard(), NA)
  expect_equal(read_clipboard(), NA) # previously ""

  # finally test tibble
  skip_if_not_installed("tibble")
  expect_equal(read_clipboard("tibble"), tibble::as_tibble(x))
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
