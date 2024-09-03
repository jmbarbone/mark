test_that("merge_list() works", {
  x <- list(a = 1, b = 2,    c = NULL, d = NULL)
  y <- list(a = 2, b = NULL, c = 3)

  obj <- merge_list(x, y)
  exp <- list(a = 1, b = 2, c = 3, d = NULL)
  expect_identical(obj, exp)

  obj <- merge_list(x, y, keep = "y")
  exp <- list(a = 2, b = 2, c = 3, d = NULL)
  expect_identical(obj, exp)

  obj <- merge_list(x, y, null = "drop")
  exp <- list(a = 1, b = 2, c = 3)
  expect_identical(obj, exp)

  obj <- merge_list(x, y, keep = "y", null = "keep")
  exp <- list(a = 2, b = NULL, c = 3, d = NULL)
  expect_identical(obj, exp)
})
