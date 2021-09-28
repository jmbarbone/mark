test_that("insert() works", {
  res <- insert(letters[1:5], c(2, 4), c("X", "Y"))
  exp <- c("a", "X", "b", "c", "Y", "d", "e")
  expect_identical(res, exp)
})
