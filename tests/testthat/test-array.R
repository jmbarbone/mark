test_that("array_extract()", {
  x <- array(rep(NA, 27), dim = c(3, 3, 3))
  x[1, 2, 3] <- TRUE
  expect_true(array_extract(x, `2` = 2, `3` = 3))
  val <- x
  expect_true(array_extract(val, `2` = 2, `3` = 3))
})
