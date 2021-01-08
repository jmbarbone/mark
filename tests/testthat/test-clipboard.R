test_that("clipboard", {
  skip_if_not(is_windows(), "Is Windows?")

  cb <- read_clipboard()
  on.exit(write_clipboard(cb), add = TRUE)
  clear_clipboard()

  test_clipboard <- function(x) {
    expect_error(write_clipboard(x), NA)
    expect_equal(read_clipboard(), x)
  }

  test_clipboard(c(TRUE, FALSE, NA))
  test_clipboard(runif(5))
  test_clipboard(-1:4)
  test_clipboard(as.Date("2020-01-02") + 0:4)
  test_clipboard(runif(1e6))

  x <- data.frame(
    var1 = 1:3,
    var2 = letters[1:3],
    var3 = as.Date("2020-01-03") + 1:3,
    var4 = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )

  expect_error(write_clipboard(x), NA)
  expect_equal(read_clipboard("data.frame"), x)
  expect_equal(read_clipboard("tibble"), tibble::as_tibble(x))

  expect_error(clear_clipboard(), NA)
  expect_equal(read_clipboard(), "")
})
