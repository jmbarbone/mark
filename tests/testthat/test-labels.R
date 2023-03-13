test_that("default assignment", {
  x0 <- c(0.350, 0.992, 0.112, 0.735, 0.598, 0.178, 0.195, 0.766, 0.816, 0.574)
  x <- assign_labels(x0, "runs")
  x1 <- remove_labels(x)

  expect_false(inherits(x, "labelled")) # Hmisc::label produces this class
  expect_equal(exattr(x, "label"), "runs")

  expect_error(assign_labels(x, NULL))
  expect_error(assign_labels(x, 1:2))

  expect_equal(x0, x1)
  expect_true(is.null(attr(x1, "label")))
})

test_that("data.frame assignment", {
  x0 <- head(iris)
  x <- assign_labels(x0, Sepal.Length = "a", Species = "b")

  exp <- quick_dfl(
    column = colnames(x0),
    label = c("a", NA, NA, NA, "b")
  )

  exp0 <- remove_labels(x, "Species")
  exp1 <- remove_labels(x, c("Sepal.Length", "Species"))
  # removes all columns -- shouldn't throw an error
  exp2 <- remove_labels(x)

  expect_equal(get_labels(x), exp)

  expect_error(
    assign_labels(x0, a = "x", b = "y", `1` = 2),
    "Columns not found: a, b, 1",
    class = "assignLabelsDataframeMissingError"
  )

  expect_error(
    assign_labels(x0, NULL),
    class = "assignLabelsDataframeNamesError"
  )

  expect_true(is.null(attr(exp0[["Species"]], "label")))
  expect_equal(attr(exp0[["Sepal.Length"]], "label"), "a")
  expect_equal(x0, exp2)
  expect_equal(exp1, exp2)

  expect_error(assign_labels(x0, .ls = list()), class = "simpleError")
  expect_error(
    assign_labels(x0, a = 1, .ls = list(b = 2)),
    class = "assignLabelsDataframeDotsError"
  )

  df <- data.frame(a = 1, b = 2, c = 3)
  # nolint start: line_length_linter.
  expect_error(assign_labels(df, c = "c", d = "d", .missing = "error"), class = "assignLabelsDataframeMissingError")
  expect_warning(assign_labels(df, c = "c", d = "d", .missing = "warn"), class = "assignLabelsDataframeMissingWarning")
  expect_warning(assign_labels(df, c = "c", d = "d", .missing = "skip"), NA)
  # nolint end: line_length_linter.
})

test_that("data.frame assign with data.frame", {
  op <- options(stringsAsFactors = FALSE)

  x <- assign_labels(iris, Sepal.Length = "a", Species = "b")

  labels <- data.frame(
    name = c("Sepal.Length", "Species"),
    label = c("a", "b")
  )

  y <- assign_labels(iris, labels)

  exp <- data.frame(
    column = colnames(iris),
    label = c("a", NA, NA, NA, "b")
  )

  expect_equal(get_labels(y), get_labels(y))

  bad_labels <- data.frame(
    v1 = c("a", "b", 1),
    v2 = c("x", "y", 2)
  )

  expect_error(
    assign_labels(iris, bad_labels),
    "Columns not found: a, b, 1",
    class = "assignLabelsDataframeMissingError"
  )

  options(op)
})

test_that("view_labels() works", {
  df <- data.frame(a = 1, b = 2)
  df <- assign_labels(df, a = "a", b = "b")

  # redefine "View"
  View <- function(x, ...) identity(x) # nolint: object_name_linter.
  expect_error(view_labels(df), NA)

  View <- NA # nolint: object_name_linter.
  expect_error(view_labels(df), class = "viewLabelsSomethingError")
})

test_that("exact match [141]", {
  x <- struct(1L, "integer", labels = 1:2)
  expect_identical(get_labels(x), NA_character_)
})
