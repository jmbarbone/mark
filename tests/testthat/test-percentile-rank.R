test_that("percentile_rank() works", {
  x <- c(2, 2, 3, 1, 0, 1, 2, 2)
  res <- percentile_rank(x)
  exp <- c(0.0625, .25, .625, .9375)
  names(exp) <- 0:3
  exp <- exp[match(x, 0:3)]
  expect_equal(res, exp)

  res <- percentile_rank(7:1, c(1, 0, 2, 2, 3, 1, 1))
  exp <- c(95, 90, 80, 60, 35, 15, 5) / 100
  names(exp) <- 7:1
  expect_equal(res, exp, tolerance = .0455)
})

test_that("percentile_rank_weighted() handles decimals [92]", {
  x <- c(1, 3.120000000001, 3.120000000001, 4)
  w <- 1:4
  expect_error(percentile_rank(x, w), class = "dupeCheckError")
})
