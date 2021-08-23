test_that("unlist0() works", {
  x <- list(a = 1:2, b = 3L, a = 4L)

  expect_identical(
    unlist0(x),
    set_names0(1:4, c("a", "a", "b", "a"))
  )
})
