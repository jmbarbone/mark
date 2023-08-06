test_that("merge_list() works", {
  x <- list(a = 1, b = 2, c = NULL)
  y <-  list(a = 2, c = 3)

  obj <- merge_list(x, y)
  exp <- list(a = 1, b = 2, c = 3)
  expect_identical(obj, exp)

  obj <- merge_list(x, y, keep = "y")
  # TODO should this be sorted?
  exp <- list(a = 2, b = 2, c = 3)
  expect_identical(obj, exp)
})
