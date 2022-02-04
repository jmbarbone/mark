test_that("percentile_rank() works", {
  res <- percentile_rank(c(2, 2, 3, 1, 0, 1, 2, 2))
  exp <- c(`2` = 0.625, `2` = 0.625, `3` = 0.9375, `1` = 0.25, `0` = 0.0625, `1` = 0.25, `2` = 0.625, `2` = 0.625)
  expect_equal(res, exp)

  res <- percentile_rank(7:1, c(1, 0, 2, 2, 3, 1, 1))
  exp <- c(95, 95, 80, 60, 35, 15, 5) / 100
  names(exp) <- 7:1
  expect_equal(res, exp, tolerance = .0455)
})

test_that("percentile_rank_weighted() handles decimals [92]", {
  x <- c(1, 3.120000000001, 3.120000000001, 4)
  w <- 1:4
  expect_error(percentile_rank(x, w), "Duplicate")
})
