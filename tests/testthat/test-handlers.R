test_that("handler examples", {
  has_catch_c <- function(...) {
    x <- c(list(...))
    class(x) <- c("has_catch", "logical")
    x
  }

  expect_identical(
    has_warning(c(1, "no"), as.integer),
    has_catch_c(`1` = FALSE, no = TRUE),
    ignore_attr = TRUE
  )

  expect_identical(
    get_warning(c(1, "no"), as.integer),
    list(`1` = NULL, no = "NAs introduced by coercion")
  )

  expect_identical(
    get_warning(c(1, "no"), as.integer, .null = FALSE),
    list(no = "NAs introduced by coercion")
  )


  foo <- function(x) {
    stopifnot(x > 0)
    x
  }

  expect_identical(
    has_error(c(1, 0, 2), foo),
    has_catch_c(`1` = FALSE, `0` = TRUE, `2` = FALSE),
    ignore_attr = TRUE
  )

  expect_identical(
    get_error(c(1, 0, 2), foo),
     list(`1` = NULL, `0` = "x > 0 is not TRUE", `2` = NULL)
  )

  expect_identical(
    get_error(c(1, 0, 2), foo, .null = FALSE),
    list(`0` = "x > 0 is not TRUE")
  )

  expect_identical(
    get_warning(c(1, "no"), as.integer),
    list(`1` = NULL, no = "NAs introduced by coercion")
  )

  message_foo <- function(x) {
    if (x > 2) {
      message("This is a message")
    }
    x
  }

  expect_identical(
    get_message(c(1, 2, 3), message_foo),
    has_catch_c(`1` = NULL, `2` = NULL, `3` = "This is a message\n"),
    ignore_attr = TRUE
  )

  expect_identical(
    has_message(c(1, 2, 3), message_foo),
    has_catch_c(`1` = FALSE, `2` = FALSE, `3` = TRUE),
    ignore_attr = TRUE
  )
})

