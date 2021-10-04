test_that("are_identical() works", {
  x <- y <- z <- 1:5
  y[2] <- 3L
  z[5] <- NA_integer_


  res <- are_identical(x, y)
  exp <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
  expect_identical(res, exp)

  res <- are_identical(x, y, z)
  exp <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_identical(res, exp)
})

test_that("are_identical() fails", {
  expect_error(are_identical(1, 1:3))
  expect_error(are_identical(NULL))
})