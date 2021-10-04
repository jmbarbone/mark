test_that("median2() works", {
  x <- runif(100)
  expect_equal(median2(x), median(x))
  expect_equal(
    median2(x, type = 3),
    quantile(x, type = 3, probs = .5, names = FALSE)
  )
})

test_that("range2() works", {
  x <- runif(100)
  expect_equal(range2(x), c(min(x), max(x)))
})
