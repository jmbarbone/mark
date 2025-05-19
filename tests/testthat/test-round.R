test_that("round_to() works", {
  x <- c(1L, 0L, 2L, 1L, 0L, 0L, 2L, 1L, 0L, 0L)
  exp <- c(0.5, 0, 1.5, 0.5, 0, 0, 1.5, 0.5, 0, 0)
  expect_identical(round_to(x, c(0, 0.5, 1.5, 3)), exp)
})
