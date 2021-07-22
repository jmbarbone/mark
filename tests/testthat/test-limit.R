test_that("limit() works", {
  expect_equal(limit(1:6, 2, 4), c(2, 2, 3, 4, 4, 4))
  expect_equal(limit(1:6, 2L, 4L), c(2L, 2L, 3L, 4L, 4L, 4L))
})

test_that("limit() errors", {
  expect_error(limit(1, "a", 1), "lower must be a single numeric value")
  expect_error(limit(1, 1:2, 1), "lower must be a single numeric value")
  expect_error(limit(1, 1, "a"), "upper must be a single numeric value")
  expect_error(limit(1, 1, 1:2), "upper must be a single numeric value")
  expect_error(limit(1, 2, 1), "lower cannot be more than upper")
})
