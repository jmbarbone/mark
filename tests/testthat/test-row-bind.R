test_that("row_bind() works", {
  res1 <- row_bind(quick_dfl(a = 1, b = 2), quick_dfl(b = 2, c = 3))
  res2 <- row_bind(list(quick_dfl(a = 1, b = 2), quick_dfl(b = 2, c = 3)))
  exp <- quick_dfl(a = c(1L, NA), b = c(2L, 2L), c = c(NA, 3L))
  expect_identical(res1, exp)
  expect_identical(res2, exp)

  expect_error(row_bind(1))
  expect_identical(row_bind(list()), quick_df(NULL))
})

test_that("row_bind() actually works [66]", {
  x <- list(quick_dfl(a = 1:2, b = 1:2), quick_dfl(b = 3, c = 4))
  res <- row_bind(x)
  exp <- quick_dfl(a = c(1:2, NA), b = 1:3, c = c(NA, NA, 4L))
  expect_identical(res, exp)
})
