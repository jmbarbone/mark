context("Labels")

test_that("default assignment", {
  x <- runif(10)
  x <- assign_label(x, "runs")
  expect_false(inherits(x, "labelled")) # Hmisc::label produces this class
  expect_equal(attr(x, "label"), "runs")
})

test_that("data.frame assignment", {
  x <- assign_label(iris, Sepal.Length = "a", Species = "b")
  exp <- data.frame(column = colnames(iris),
                    label = c("a", NA, NA, NA, "b"))
  expect_equal(get_labels(x), exp)

  expect_error(assign_label(iris, a = "x", b = "y", `1` = 2),
               "Columns not found: a, b, 1")
})
