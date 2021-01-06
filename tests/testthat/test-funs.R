test_that("within_*()", {
  FOO <- function() foo()
  foo <- function(x) within_call()

  expect_equal(foo(), "foo()")

  foo <- function() within_fun()
  foo2 <- function() foo()

  expect_equal(foo(), "foo")
  expect_equal(FOO(), "foo")
  # Should return the function that immediately contains within_fun()
  expect_equal(foo2(), "foo") # not foo2
  expect_equal(FOO(), "foo") # not FOO
})

test_that("outer_*()", {
  bar <- function() outer_call()
  foo <- function() bar()

  expect_equal(foo(), "foo()")
  bar <- function() outer_fun()
  expect_equal(foo(), "foo")
})
