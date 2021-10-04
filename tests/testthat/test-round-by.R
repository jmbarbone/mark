test_that("round_by() works", {
  x <- seq(1, 13, by = 4/3)

  res <- round_by(x, 1)
  exp <- c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13)
  expect_equal(res, exp)

  res <- round_by(x, 2)
  exp <- c(0, 2, 4, 4, 6, 8, 8, 10, 12, 12)
  expect_equal(res, exp)

  res <- round_by(x, 2, include0 = FALSE)
  exp <- c(2, 2, 4, 4, 6, 8, 8, 10, 12, 12)
  expect_equal(res, exp)
})
