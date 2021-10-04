test_that("mapply0() works", {

  foo <- function(a, b, c) sum(a, b, c)
  res <- mark:::mapply0(foo, a = 1:2, b = NULL, c = 3)
  exp <- list(4, 5)
  expect_identical(exp, res)

  x <- list(a = 1:2, b = NULL, c = 3)
  res <- mark:::mapply0(foo, x)
  expect_identical(exp, res)
})
