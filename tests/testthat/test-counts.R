
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

  expect_equal(
    counts(c(NA, NA, 1)),
    set_names0(c(1, 2), c(1, NA))
  )

  expect_equal(
    counts(c("a", NA, NA)),
    set_names0(c(1, 2), c("a", NA))
  )

  expect_equal(
    counts(c(NA_real_, NA_real_)),
    set_names0(2, NA)
  )
})

test_that("data.frame", {
  df <- data.frame(a = rep("x", 3), b = factor(c("a", "a", "b")))

  expect_equal(
    counts(df, 1),
    data.frame(a = "x", freq = 3)
  )

  expect_equal(
    # TODO this needs to be remade as a factor
    counts(df, 2),
    data.frame(b = factor(c("a", "b")), freq = 2:1)
  )

  expect_equal(
    counts(df, 1:2),
    data.frame(
      a = rep("x", 2),
      b = factor(c("a", "b")),
      freq = 2:1
    )
  )
})
