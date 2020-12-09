test_that("trailing zeros", {
  x <- c(0.90001, 0.00999)
  res <- c("0.900", "0.010")
  expect_equal(p_round(x), res)
})

test_that("below threshold", {
  x <- c(0.001, 0.0010, 0.00099, 0.00001)
  res <- c("0.001", "0.001", "<.001", "<.001")
  expect_equal(p_round(x), res)

  x <- c(0.0001, 0.00010, 0.000099, 0.000001)
  res <- c("0.0001", "0.0001", "<.0001", "<.0001")
  expect_equal(p_round(x, n = 4), res)
})

