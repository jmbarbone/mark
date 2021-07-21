test_that("char2fact() works", {
  expect_error(char2fact(1L))

  x <- letters[c(1, 2, 2, 2, 3, 4, 4, 5, 5)]
  expect_equal(char2fact(x), fact(x))

  # Threshold
  y <- c(x, "j")
  expect_equal(y, char2fact(y, 5))
  expect_equal(fact(y), char2fact(y, 6))
})

test_that("char2fact.data.frame() works", {
  op <- options(stringsAsFactors = FALSE)

  x <- data.frame(
    a = letters[c(1, 2, 2, 2, 3, 4, 4, 5, 5)],
    b = letters[c(1, 2, 3, 4, 5, 6, 7, 8, 9)]
  )

  y <- x
  y[["a"]] <- fact(y[["a"]])

  expect_equal(char2fact(x), y)

  options(op)
})
