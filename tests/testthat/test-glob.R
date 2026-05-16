test_that("glob() works", {
  x <- c("apple", "banana", "peach", "pear", "orange")
  obj <- glob(x, "*e")
  exp <- c("apple", "orange")
  expect_identical(obj, exp)

  obj <- glob(x, "pea*", value = FALSE)
  exp <- c(3L, 4L)
  expect_identical(obj, exp)

  obj <- glob(x, "*an*", value = NA)
  exp <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
  expect_identical(obj, exp)
})
