test_that("to_boolean.logical() works", {
  x <- c(TRUE, TRUE, FALSE, FALSE, NA)
  expect_identical(to_boolean(x), x)
})

test_that("to_boolean.numeric() works", {
  expect_identical(
    to_boolean(c(1, 2, -1, 0, NA, 3)),
    c(TRUE, NA, NA, FALSE, NA, NA)
  )
})

wtf <- c("what", "true", "T", "TRUE", "false", "F", "false")
yn <- c("Y", "yes", "N", "no")
test_that("to_boolean.character() works", {
  expect_identical(
    to_boolean(wtf),
    c(NA, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    to_boolean(yn),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("to_boolean.factor() works", {
  expect_identical(
    to_boolean(fact(wtf)),
    c(NA, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    to_boolean(fact(yn)),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

