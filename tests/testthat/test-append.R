
test_that("append0() works", {
  expect_identical(append0(1:3, 4L), 1:4)
  expect_identical(append0(1:3, 4L, 2), c(1L, 4L, 2L, 3L))
})

test_that("append0.list() works", {
  expect_identical(append0(list(1:3), 4L), list(1:3, 4L))
  expect_identical(append0(list(a = 1:3), list(b = 4L)), list(a = 1:3, b = 4L))

  expect_identical(
    append0(list(a = 1:3), list(b = 4), 0L),
    list(b = 4, a = 1:3)
  )

  expect_identical(
    append0(list(a = 1:3, c = TRUE), list(b = 4), 1L),
    list(b = 4, a = 1:3, c = TRUE)
  )

  expect_identical(
    append0(list(a = 1:3, c = TRUE), list(b = 4), 2L),
    list(a = 1:3, b = 4, c = TRUE)
  )
})

test_that("append.data.frame() works", {
  x <- quick_dfl(a = 1:2, b = 0:1)
  res <- append0(x, list(c = c(TRUE, FALSE)))
  exp <- quick_dfl(a = 1:2, b = 0:1, c = c(TRUE, FALSE))
  expect_identical(res, exp)

  res <- append0(x, list(c = c(TRUE, FALSE)), 2)
  exp <- quick_dfl(a = 1:2, c = c(TRUE, FALSE), b = 0:1)
  expect_identical(res, exp)
})
