test_that("round_by() works", {
  x <- seq(1, 13, by = 4/3)

  exp <- round_by(x, 1)
  res <- c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13)
  expect_equal(exp, res)

  exp <- round_by(x, 2)
  res <- c(0, 2, 4, 4, 6, 8, 8, 10, 12, 12)
  expect_equal(exp, res)
})
