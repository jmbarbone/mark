test_that("sort-by() works", {
  x <- runif(10)
  y <- runif(10)

  expect_equal(sort_by(x, x), sort(x))
  expect_equal(sort_by(x, y), x[order(y)])
  expect_equal(sort_by(x, -y), x[flip(order(y))])
})
