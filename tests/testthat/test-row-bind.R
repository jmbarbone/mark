test_that("row_bind() works", {
  res1 <- row_bind(quick_dfl(a = 1, b = 2), quick_dfl(b = 2, c = 3))
  res2 <- row_bind(list(quick_dfl(a = 1, b = 2), quick_dfl(b = 2, c = 3)))
  exp <- quick_dfl(a = c(1L, NA), b = c(2L, 2L), c = c(NA, 3L))
  expect_identical(res1, exp)
  expect_identical(res2, exp)

  expect_error(row_bind(1))
  expect_identical(row_bind(list()), quick_df(NULL))
})
