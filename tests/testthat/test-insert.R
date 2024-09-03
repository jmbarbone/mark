
test_that("insert.default() works", {
  res <- insert(letters[1:5], c(2, 4), c("X", "Y"))
  exp <- c("a", "X", "b", "c", "Y", "d", "e")
  expect_identical(res, exp)

  res <- insert(letters[1:4], c(1, 3), "z")
  exp <- c("z", "a", "b", "z", "c", "d")
  expect_identical(res, exp)

  expect_error(insert(1, NA, 1), class = "simpleError")
  expect_error(insert(1, integer(), 1), class = "insertNposError")
  expect_error(insert(1, 1, integer()), class = "insertLengthError")
})

test_that("insert.data.frame() works", {
  x <- quick_dfl(
    a = c(1, 2, 3),
    b = c("a", "b", "c"),
    c = c(-1.5, 0, 1.5)
  )

  exp <- quick_dfl(
    a = c(1, 2, 3),
    x = 1:3,
    b = c("a", "b", "c"),
    c = c(-1.5, 0, 1.5)
  )

  res <- insert(x, 2, list(x = 1:3))
  expect_identical(res, exp)
})
