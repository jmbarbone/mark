test_that("expand_by() work", {
  x <- letters[c(3:2, 5, 9)]
  y <- letters[c(1:4, 8)]

  res <- expand_by(x, y, "x")
  exp <- c(c = "c", b = "b", e = NA, i = NA)
  expect_equal(res, exp)

  res <- expand_by(x, y, "y")
  # what?  shit, that's not right
  exp <- c(a = NA, b = "b", c = "c", d = NA, h = NA)
  expect_equal(res, exp)

  res <- expand_by(x, y, "intersect")
  exp <- c(b = "b", c = "c")
  expect_equal(exp, res)

  res <- expand_by(x, y, "both")
  exp <- c(c = "c", b = "b", e = "e", i = "i", a = NA, d = NA, h = NA)
  expect_equal(exp, res)

  res <- expand_by(x, y, "both", sort = TRUE)
  exp <- sort_names(
    c(c = "c", b = "b", e = "e", i = "i", a = NA, d = NA, h = NA)
  )
  expect_equal(exp, res)

  # nolint start: line_length_linter.
  expect_error(expect_warning(expand_by(c(a = 1, a = 1), c(a = 1))), class = "simpleError")
  expect_error(expect_warning(expand_by(c(a = 1), c(a = 1, a = 1))), class = "simpleError")
  # nolint end: line_length_linter.
})

test_that("reindex() work", {
  iris1 <- head(iris, 5)
  iris1$index <- 1:5
  res <- reindex(iris1, "index", seq.int(2, 8, 2))
  exp <- iris1[c(2, 4), ]
  # row name attributes mode change
  expect_equal(res, exp, ignore_attr = TRUE)

  res <- reindex(iris1, "index", seq(2, 8, 2), expand = "both")
  exp <- iris1[seq(2:8), ]
  expect_equal(res, exp, ignore_attr = TRUE)

  #' # Using letters will show changes in rownames
  iris1$index <- letters[1:5]
  reindex(iris1, "index", letters[seq.int(2, 8, 2)])

  reindex(iris1, "index", seq(2, 8, 2))
  reindex(iris1, "index", seq(2, 8, 2), expand = "both")

  expect_error(reindex(1), "data.frame", class = "simpleError")
  expect_error(
    reindex(data.frame(a = 1), index = integer()),
    "new_index",
    class = "simpleError"
  )
})

test_that("expand helpers work", {
  expect_warning(unique_name_check(c(a = 1, a = 2)))
})
