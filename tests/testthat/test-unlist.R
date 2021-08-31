test_that("unlist0() works", {
  x <- list(a = 1:2, b = 3L, a = 4L)

  expect_identical(
    unlist0(x),
    set_names0(1:4, c("a", "a", "b", "a"))
  )
})

test_that("sqash_vec() works", {
  x <- list(a = 1:3, b = 2, c = 2:4)
  y <- c(a = 1, b = 1, c = 1, d = 2, e = 3, f = 3)
  expect_identical(squash_vec(x), c(a = 1, a.b.c = 2, a.c = 3, c = 4))
  expect_identical(squash_vec(y), c(a.b.c = 1, d = 2, e.f = 3))
  expect_identical(squash_vec(y, sep = "_"), c(a_b_c = 1, d = 2, e_f = 3))
})