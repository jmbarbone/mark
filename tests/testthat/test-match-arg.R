test_that("match-param", {
  foo <- function(x = NULL, y = c(1, 2, 3)) {
    y <- match_param(y)
    x
  }

  expect_null(foo())
  expect_null(foo(y = 1))
  expect_error(
    foo(y = 4),
    '`match_param(y)` failed in `foo(y = 4)`:
  `y` [4] must be one of the following: "1", "2", "3"',
    fixed = TRUE
  )

  foo2 <- function(y = 1:3) {
    match_param(y)
  }

  expect_equal(foo2(1), 1)
  expect_equal(foo2(1L), 1L)
  expect_equal(foo2("1"), 1)


  foo <- function(x = NULL) {
    match_param(tolower(x), c("a", "b", "c"))
  }

  expect_error(
    foo(),
    '`match_param(tolower(x))` failed in `foo()`:
  `tolower(x)` [character(0)] must be one of the following: "a", "b", "c"',
    fixed = TRUE
  )

  foo <- function(x = c("a", "b"), null = FALSE) {
    match_param(x, null = null)
  }

  expect_error(foo(NULL), "non-NULL")
  expect_null(foo(NULL, null = TRUE))
})

test_that("match_arg() works", {
  foo <- function(x = c("a", "b"), table) {
    match_arg(x, table)
  }

  expect_null(foo(NULL))
  expect_identical(foo(), "a")
  expect_identical(foo(table = "a"), "a")
  expect_error(foo(table = "c"))
})
