test_that("unlist0() works", {
  x <- list(a = 1:2, b = 3L, a = 4L)

  expect_identical(
    unlist0(x),
    set_names(1:4, c("a", "a", "b", "a"))
  )

  # should be fine without names
  expect_identical(unlist0(list(1:3)), 1:3)
})

test_that("sqash_vec() works", {
  x <- list(a = 1:3, b = 2, c = 2:4)
  y <- c(a = 1, b = 1, c = 1, d = 2, e = 3, f = 3)

  expect_identical(squash_vec(x), c(a = 1, a.b.c = 2, a.c = 3, c = 4))
  expect_identical(squash_vec(y), c(a.b.c = 1, d = 2, e.f = 3))
  expect_identical(squash_vec(y, sep = "_"), c(a_b_c = 1, d = 2, e_f = 3))

  x <- c(a = "a", b1 = "b", b2 = "b", c1 = "c", c2 = "c", c3 = "c")
  exp <- c(a = "a", b1.b2 = "b", c1.c2.c3 = "c")
  expect_identical(squash_vec(x), exp)

  x <- c(b1 = "b", c3 = "c", c1 = "c", a = "a", b2 = "b", c2 = "c")
  exp <- c(b1.b2 = "b", c3.c1.c2 = "c", a = "a")
  expect_identical(squash_vec(x), exp)
})
