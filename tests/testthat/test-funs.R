test_that("within_*()", {
  FOO <- function() foo() # nolint: object_name_linter.
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

test_that("require_namespace()", {
  expect_error(
    require_namespace("impossible package"),
    class = "namespaceError"
  )

  foo <- function() {
    require_namespace("not-real")
  }

  bar <- function() {
    foo()
  }

  expect_error(foo(), class = "namespaceError")
  expect_error(bar(), class = "namespaceError")

  expect_error(
    require_namespace("this_one", "that_thing"),
    class = "namespaceError"
  )
})

test_that("quiet_stop()", {
  expect_error(quiet_stop(), NULL)

  foo <- function(x) {
    tryCatch(quiet_stop(),
             error = function(e) {
               !is.null(e$message)
             })
  }

  bar <- function(x) {
    tryCatch(quiet_stop(),
             error = function(e) {
               warning(e$message)
             })
  }

  # Message exists
  expect_true(foo())
  expect_warning(bar())
})

test_that("other funs", {
  expect_error(rn_soft("bad package name"), NULL)
  expect_true(package_available("methods"))
})
