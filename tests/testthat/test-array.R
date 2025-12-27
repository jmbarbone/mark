test_that("array_extract() works", {
  x <- array(rep(NA, 27), dim = c(3, 3, 3))
  x[1, 2, 3] <- TRUE
  expect_deprecated(expect_true(array_extract(x, `2` = 2, `3` = 3)))
  val <- x
  expect_deprecated(expect_true(array_extract(val, `2` = 2, `3` = 3)))

  x <- array(1:9, dim = c(3, 3))
  dimnames(x) <- list(one = LETTERS[1:3], two = letters[1:3])

  expect_deprecated(expect_identical(array_extract(x, 1, 2), x[1, 2]))
  expect_deprecated(expect_deprecated(
    expect_identical(array_extract(x, "A", "b"), array_extract(x, 1, 2))
  ))
  y <- fact(c("A", "b"))
  expect_deprecated(expect_deprecated(
    expect_identical(
      array_extract(x, y[1], y[2]),
      array_extract(x, 1, 2)
    )
  ))
})

test_that("array_extract() errors", {
  x <- array(1:9, dim = c(3, 3))
  expect_deprecated(expect_error(array_extract(1), class = "simpleError"))
  expect_deprecated(array_extract(x, 2, 3))
  expect_deprecated(expect_error(
    array_extract(x, a = 2, b = 3),
    class = "mark:input_error"
  ))
})
