test_that("insert() works", {
  res <- insert(letters[1:5], c(2, 4), c("X", "Y"))
  exp <- c("a", "X", "b", "c", "Y", "d", "e")
  expect_identical(res, exp)

  res <- insert(letters[1:4], c(1, 3), "z")
  exp <- c("z", "a", "b", "z", "c", "d")
  expect_identical(res, exp)

  expect_error(insert(1, NA, 1))
  expect_error(insert(1, integer(), 1))
  expect_error(insert(1, 1, integer()))
})
