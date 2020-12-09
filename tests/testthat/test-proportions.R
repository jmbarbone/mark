test_that("Proportions", {
  # proportion.default
  res <- proportion(iris$Species)
  exp <- c(1, 1, 1) / 3
  names(exp) <- c("setosa", "versicolor", "virginica")
  expect_equal(res, exp)

  # proportion.data.frame
  res <- proportion(iris, "Species")
  exp <- vector2df(exp, "Species", "prop")
  expect_equal(res, exp)

  # TODO test for ordering
  # TODO test for ordered factor
  # TODO test for numeric
})