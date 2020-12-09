test_that("reverse", {
  expect_equal(reverse(letters), letters[26:1])
  x <- set_names(1:5, letters[1:5])
  expect_named(reverse(x))
  expect_equal(names(reverse(x)), letters[5:1])

  res1 <- iris[6:1, ]
  res2 <- res1
  rownames(res1) <- NULL
  iris2 <- res1
  rownames(iris2) <- letters[1:6]
  res3 <- iris2[6:1, ]

  expect_equal(reverse(head(iris)), res1)
  expect_equal(reverse(head(iris), keep_rownames = TRUE), res2)
  expect_equal(reverse(iris2), res3)
  expect_equal(reverse(iris2, keep_rownames = TRUE), res3)
})
