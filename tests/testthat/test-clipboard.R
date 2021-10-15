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
  expect_equal(read_clipboard("tibble"), tibble::as_tibble(x))

  expect_error(clear_clipboard(), NA)
  expect_equal(read_clipboard(), "")
})

test_that("try_vectors_formats()", {
  xchr <- c("this", "that", NA, "121", "them", ".011", "2020", "NA")
  xdbl <- c("-0.10", "  .10123", "-.102010000  ", "NaN")
  xint <- c("121021", "-12191", "121001", "  ")
  xdat <- c("2020-05-01", "1900-10-10", "1655-06-07")
  xlgl <- c("TRUE", "  true", "FALSE", "fALSE", "na", "NA")
  rchr <- xchr
  rdbl <- as.double(xdbl)
  rint <- as.integer(xint)
  rdat <- as.Date(xdat)
  rlgl <- as.logical(toupper(xlgl))

  expect_equal(try_vector_formats(xchr), rchr)
  expect_equal(try_vector_formats(xdbl), rdbl)
  expect_equal(try_vector_formats(rdbl), rdbl)
  expect_equal(try_vector_formats(xint), rint)
  expect_equal(try_vector_formats(rint), rint)
  expect_equal(try_vector_formats(xdat), rdat)
  expect_equal(try_vector_formats(rdat), rdat)
  expect_equal(try_vector_formats(xlgl), rlgl)
  expect_equal(try_vector_formats(rlgl), rlgl)
})
