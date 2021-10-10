test_that("simpleTimeReport() works", {
  expect_error(
    expect_warning(
      simpleTimeReport("title goes here", {
        1 + 2
        foo_warn <- function() warning("this is a warning")
        foo_msg <- function() message("this is a message")
        foo_warn()
        foo_msg()
        cat("this is a cat\n")
        data.frame(a = 1, b = 2)
      })
    ),
  NA)
})
