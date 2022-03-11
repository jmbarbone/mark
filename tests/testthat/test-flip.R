test_that("flip.default", {
  expect_equal(flip(letters), letters[26:1])
  x <- set_names0(1:5, letters[1:5])
  expect_named(flip(x))
  expect_equal(names(flip(x)), letters[5:1])

  expect_identical(flip(1), 1)
})

test_that("flip.data.frame", {
  res1 <- iris[6:1, ]
  res2 <- res1
  rownames(res1) <- NULL
  iris2 <- res1
  rownames(iris2) <- letters[1:6]
  res3 <- iris2[6:1, ]

  expect_equal(flip(head(iris)), res1)
  expect_equal(flip(head(iris), keep_rownames = TRUE), res2)
  expect_equal(flip(iris2), res3)
  expect_equal(flip(iris2, keep_rownames = TRUE), res3)

  # test with 0 cols
  df <- quick_df(NULL)
  expect_identical(flip(df), df)

  df <- quick_df(list(a = 1))
  df$a <- NULL
  expect_identical(flip(df), df)
})

test_that("flip.matrix", {
  mat <- matrix(1:25, ncol = 5)
  res1 <- mat[5:1, ]
  expect_equal(flip(mat), res1)

  res2 <- mat[, 5:1]
  expect_equal(flip(mat, by_row = FALSE), res2)

  mat2 <- mat
  dimnames(mat2) <- list(letters[1:5])
  res3 <- mat2[5:1, ]
  expect_equal(flip(mat2), res3)

  mat <- matrix(1)
  expect_identical(flip(mat), mat)

  mat <- matrix(1:2, dimnames = list(row = letters[1:2], col = "1"))
  res <- flip(mat, keep_rownames = FALSE)
  exp <- matrix(2:1, dimnames = list(row = letters[1:2], col = "1"))
  expect_identical(res, exp)

  mat <- structure(integer(), class = "matrix")
  expect_identical(flip(mat, by_row = FALSE), mat)
})

test_that("flip doesn't coerce into lower object [36]", {
  x <- data.frame(x = 1)
  expect_identical(x, flip(x))
  expect_identical(x, flip(x, by_row = FALSE))

  x <- matrix(1:4, nrow = 2)
  expect_identical(dim(flip(x)), c(2L, 2L))
  expect_identical(dim(flip(x, by_row = FALSE)), c(2L, 2L))

  expect_identical(flip(matrix(1)), matrix(1))
})

