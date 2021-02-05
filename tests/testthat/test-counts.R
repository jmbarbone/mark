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

  x <- c(1L, 0L, NA_integer_)
  res <- set_names0(c(1L, 1L, 1L), x)
  expect_equal(counts(x), res)
})
