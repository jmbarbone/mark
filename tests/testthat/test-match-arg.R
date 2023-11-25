test_that("match_param() works", {
  foo <- function(x = NULL, y = c(1, 2, 3)) {
    y <- match_param(y)
    x
  }

  expect_null(foo())
  expect_null(foo(y = 1))
  expect_error(
    foo(y = 4),
    regexp = collapse(
      "`match_param(y)` failed in `foo(y = 4)`:",
      "  param    4",
      "  choices  1, 2, 3",
      sep = "\n"
    ),
    fixed = TRUE,
    class = "matchParamMatchError"
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
    regexp = collapse(
      "`match_param(tolower(x))` failed in `foo()`:",
      "  param    character(0)",
      "  choices  a, b, c",
      sep = "\n"
    ),
    class = "matchParamMatchError",
    fixed = TRUE
  )

  foo <- function(x = c("a", "b"), null = FALSE) {
    match_param(x, null = null)
  }

  expect_error(foo(NULL), class = "condMatchParamNullError")
  expect_null(foo(NULL, null = TRUE))
})

test_that("match_param() can partialy match", {
  fruits <- function(x = c("apple", "apricot", "banana")) {
    match_param(x, partial = TRUE)
  }

  expect_identical(fruits(), "apple")
  expect_error(fruits("a"), class = "matchParamMatchError")
  expect_identical(fruits("app"), "apple")

  fruits <- function(x = list("apple" = 1:2, "apricot" = 3, "banana" = 4)) {
    match_param(x, partial = TRUE)
  }

  expect_identical(fruits(), "apple")
  expect_error(fruits(c(a = 0)), class = "matchParamMatchError")
  expect_identical(fruits(1), "apple")
})

test_that("match_param() accepts can return multiple", {
  fruits <- function(x = c("apple", "banana", "orange")) {
    match_param(x, multiple = TRUE)
  }

  expect_identical(fruits(), c("apple", "banana", "orange"))
})

test_that("match_arg() works", {
  foo <- function(x = c("a", "b"), table) {
    match_arg(x, table)
  }

  expect_null(foo(NULL))
  expect_identical(foo(), "a")
  expect_identical(foo(table = "a"), "a")
  expect_error(foo(table = "c"), class = "condMatchArgError")
})

test_that("match_param() accepts multiple [#104]", {
  foo <- function(x = list(this = 1:2, that = 3)) {
    mark::match_param(x)
  }

  expect_identical(foo(), "this")
})

test_that("match_param() accepts unnamed multiple arguments [#219]", {
  obj <- match_param("a", list("a", b = c("c", "d")))
  exp <- "a"
  expect_identical(obj, exp)
})

test_that("match_param() accepts formula lists", {
  foo <- function(x = list(1L ~ 0:1, 2L, 3L ~ 3:5, foo = 6)) {
    match_param(x)
  }

  expect_identical(foo(1L), 1L)
  expect_identical(foo(2L), 2L)
  expect_identical(foo(5L), 3L)
  expect_identical(foo(6L), "foo")
})

test_that("match_param() finds duplicate choices", {
  expect_error(
    match_param("a", c("a", "a")),
    class = "matchParamDupesError"
  )

  expect_error(
    match_param(1, c(a = 1:2, b = 3:4, c = c(1, 3))),
    class = "matchParamDupesError"
  )
})
