test_that("is_blank() works", {
  x <- c("", NA, "  ", 1L)
  obj <- is_blank(x)
  exp <- c(TRUE, FALSE, TRUE, FALSE)
  expect_identical(obj, exp)

  obj <- is_blank(x, TRUE)
  exp <- c(TRUE, TRUE, TRUE, FALSE)
  expect_identical(obj, exp)

  obj <- is_blank(x, ws = FALSE)
  exp <- c(TRUE, FALSE, FALSE, FALSE)
  expect_identical(obj, exp)

  df <- data.frame(
    x = x,
    i = 1:4,
    na = rep(NA, 4L),
    nz = rep("", 4L),
    ws = rep(" ", 4L)
  )

  obj <- is_blank_cols(df, names = FALSE)
  exp <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  expect_identical(obj, exp)

  obj <- remove_blank_cols(df)
  exp <- df[, 1:3]
  expect_identical(obj, exp)

  obj <- select_blank_cols(df)
  exp <- df[, 4:5]
  expect_identical(obj, exp)
})
