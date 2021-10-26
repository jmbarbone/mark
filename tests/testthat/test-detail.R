test_that("details() works", {
  x <- 1:3
  y <- factor(letters[1:3])
  z <- c("x", NA_character_, "z")
  attr(z, "label") <- "information"
  df <- data.frame(x = x, y = factor(letters[1:3]))
  expect_error(detail(x), NA)
  expect_error(detail(df), NA)

  expect_error(detail(data.frame()))
})

test_that("details() keeps factors [50]", {
  expect_identical(detail(factor("a"))$class, "factor")
  expect_identical(detail(ordered("a"))$class, "ordered; factor")
})
