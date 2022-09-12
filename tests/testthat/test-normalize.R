test_that("normalize.default() works", {
  x <- 1:5
  res <- normalize(x)
  exp <- seq(0, 1, .25)
  expect_identical(res, exp)

  x <- matrix(1:5, ncol = 5)
  res <- normalize(x)
  exp <- matrix(seq(0, 1, .25), ncol = 5)
  expect_identical(res, exp)

  expect_identical(normalize(1), NaN)
  expect_identical(normalize(NA), NA_real_)
})

test_that("normalize.data.frame() works", {
  x <- data.frame(a = 1:5, b = 6:10)
  res <- normalize(x)
  exp <- data.frame(a = seq(0, 1, .25), b = seq(0, 1, .25))
  expect_identical(res, exp)
})
