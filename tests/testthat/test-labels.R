context("Labels")

test_that("default assignment", {
  x <- runif(10)
  x <- assign_label(x, "runs")
  expect_false(inherits(x, "labelled")) # Hmisc::label produces this class
  expect_equal(attr(x, "label"), "runs")

  expect_error(assign_label(x, NULL), "`label` is NULL")
  expect_error(assign_label(x, 1:2), "`label` is not of length 1L")
})

test_that("data.frame assignment", {
  x <- assign_label(iris, Sepal.Length = "a", Species = "b")
  exp <- data.frame(column = colnames(iris),
                    label = c("a", NA, NA, NA, "b"))
  expect_equal(get_labels(x), exp)
  expect_error(assign_label(iris, a = "x", b = "y", `1` = 2),
               "Columns not found: a, b, 1")

  expect_error(assign_label(iris, NULL), "`...` is NULL")
})

test_that("data.frame assign with data.frame", {
  x <- assign_label(iris, Sepal.Length = "a", Species = "b")

  labels <- data.frame(
    name = c("Sepal.Length", "Species"),
    label = c("a", "b")
  )

  y <- assign_label(iris, labels)

  exp <- data.frame(
    column = colnames(iris),
    label = c("a", NA, NA, NA, "b")
  )

  expect_equal(get_labels(y), get_labels(y))

  bad_labels <- data.frame(
    v1 = c("a", "b", 1),
    v2 = c("x", "y", 2)
  )
  expect_error(assign_label(iris, bad_labels),
               "Columns not found: a, b, 1")
})
