test_that("match param", {
  foo <- function(x = c("a", "b")) {
    match_param(x)
  }

  expect_equal(foo("a"), "a")
  expect_equal(foo("b"), "b")
  expect_equal(foo(c("a", "b")), "a")
  expect_equal(foo(c("b", "a")), "b")
  expect_error(foo("c"))
})

test_that("utils", {
  expect_true(has_length(1))
  expect_false(has_length(NULL))
  expect_false(has_length(integer()))

  expect_true(is_unique(1:2))
  expect_false(is_unique(c(1, 1)))

  res <- remove_class(struct(1, class = "foo"))
  expect_identical(class(res), "numeric")
})

test_that("check_interactive() works", {
  op <- options("mark.check_interactive")

  options(mark.check_interactive = TRUE)
  expect_identical(check_interactive(), interactive())

  options(mark.check_interactive = FALSE)
  expect_true(check_interactive())

  options(mark.check_interactive = NA)
  expect_false(check_interactive())

  options(op)
})

test_that("has_char() works", {
  expect_identical(has_char(c(NA, "this", "")), c(FALSE, TRUE,  FALSE))
  expect_identical(has_char(c(1, 2, NA)),       c(FALSE, FALSE, FALSE))
})

test_that("dupe_check() works", {
  expect_error(dupe_check(c(1, 1, 2, 3)))
  expect_error(dupe_check(1:4), NA)
})

test_that("%len%", {
  expect_true(logical() %len% TRUE)
  expect_false(FALSE %len% TRUE)
})

test_that("which0()", {
  expect_identical(which0(c(FALSE, FALSE, TRUE)), 3L)
  expect_identical(which0(c(FALSE, FALSE, FALSE)), 0L)
})

test_that("%colons%", {
  expect_error(
    "bad_package" %colons% "not_a_function",
    class = "namespaceError"
  )
})

test_that("that()", {
  expect_identical(that(c(TRUE, FALSE, TRUE)), which(c(TRUE, FALSE, TRUE)))
  expect_identical(that(c(FALSE, FALSE)), which(c(FALSE, FALSE)))
})

test_that("check_interactive()", {
  op <- options(mark.check_interactive = -1)
  expect_error(check_interactive())
  options(op)
})

test_that("try_formats() doesn't cause failure with %Z", {
  expect_false(tryCatch(
    as.POSIXct("date", tryFormats = try_formats()),
    error = function(e) {
      grepl(
        "strptime(xx, f, tz = tz)`: use of %Z for input is not supported",
        e$message,
        fixed = TRUE
      )
    }
  ))
})
