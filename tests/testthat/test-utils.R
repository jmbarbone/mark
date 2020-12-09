test_that("match param", {
  foo <- function(x = c('a', 'b')) {
    match_param(x)
  }

  expect_equal(foo("a"), "a")
  expect_equal(foo("b"), "b")
  expect_equal(foo(c("a", "b")), "a")
  expect_equal(foo(c("b", "a")), "b")
  expect_error(foo("c"))
})
