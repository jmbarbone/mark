test_that("vector counts work", {
  x <- rep(c("a", "b", "c"), c(3, 1, 4))
  res <- set_names0(c(3, 1, 4), c("a", "b", "c"))
  expect_equal(counts(x), res)

  x <- rep(c(2, 3, -1), c(3, 1, 4))
  res <- set_names0(c(3, 1, 4), c(2, 3, -1))
  expect_equal(counts(x), res)
})

test_that("counts work with NAs", {

  # Correct sort
  x <- c(FALSE, TRUE, NA)
  res <- set_names0(c(1, 1, 1), x)
  expect_equal(counts(x), res)

  x <- c("false", "true", NA_character_)
  res <- set_names0(c(1, 1, 1), x)
  expect_equal(counts(x), res)

  x <- c(1L, 0L, NA_integer_, NA_integer_)
  res <- set_names0(c(1L, 1L, 2L), c(1, 0, NA))
  expect_equal(counts(x), res)
})


test_that("can make new column name", {
  df <- data.frame(a = 1, b = 2)
  res <- counts(df, 1)
  expect_equal(colnames(res), c("a", "freq"))

  # Default to "freq"
  res <- counts(df, 1, .name = NULL)
  expect_equal(colnames(res), c("a", "freq"))

  res <- counts(df, 1, .name = "new_name")
  expect_equal(colnames(res), c("a", "new_name"))

  # Default to "prop"
  res <- props(df, 1, .name = NULL)
  expect_equal(colnames(res), c("a", "prop"))

  res <- props(df, 1)
  expect_equal(colnames(res), c("a", "prop"))

  res <- props(df, 1, .name = "new_name")
  expect_equal(colnames(res), c("a", "new_name"))
})

test_that("NAs are last", {
  expect_equal(
    counts(c(NA, NA, 1, 2)),
    set_names0(c(1, 1, 2), c(1, 2, NA))
  )
})
