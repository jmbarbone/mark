test_that("char2fact() works", {
  expect_error(char2fact(1L))

  x <- letters[c(1, 2, 2, 2, 3, 4, 4, 5, 5)]
  expect_equal(char2fact(x), fact(x))

  # Threshold
  y <- c(x, "j")
  expect_equal(y, char2fact(y, 5))
  expect_equal(fact(y), char2fact(y, 6))

  # factors
  f <- fact(x)
  expect_identical(char2fact(f), f)
})

test_that("char2fact.data.frame() works", {
  x <- quick_dfl(
    a = letters[c(1, 2, 2, 2, 3, 4, 4, 5, 5)],
    b = letters[c(1, 2, 3, 4, 5, 6, 7, 8, 9)]
  )

  y <- x
  y[["a"]] <- fact(y[["a"]])

  expect_equal(char2fact(x), y)
})

test_that("fact2char() works", {
  expect_error(fact2char(letters), class = "simpleError")

  df <- quick_dfl(
    a = 1:5,
    b = factor(1:5),
    c = letters[1:5],
    d = factor(c(1, 1, 2, 2, 3)),
    e = factor(rep(1, 5))
  )

  res <- fact2char(df, threshold = 3)
  exp <- df
  exp$b <- as.character(exp$b)
  exp$d <- as.character(exp$d)

  expect_identical(res, exp)
})
