test_that("percentile_rank() works", {
  res <- percentile_rank(7:1, c(1, 0, 2, 2, 3, 1, 1))
  exp <- c(95, 95, 80, 60, 35, 15, 5) / 100
  names(exp) <- 7:1
  expect_equal(res, exp, tolerance = .0455)
})
