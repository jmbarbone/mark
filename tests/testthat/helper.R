muffle_cnd_conditions <- function(expr) {
  withCallingHandlers(
    expr,
    "cnd::condition" = function(cond) tryInvokeRestart("muffleCondition")
  )
}

need_clipr <- function() {
  testthat::skip_if_not_installed("clipr")

  # not sure if this is needed
  withr::with_envvar(c(CLIPR_ALLOW = TRUE), {
    testthat::skip_if_not(clipr::clipr_available())
  })

  testthat::skip_on_cran()
}

with_clip <- function(code) {
  need_clipr()
  withr::with_envvar(
    c(CLIPR_ALLOW = TRUE),
    (function() {
      old <- clipr::read_clip()
      clear_clipboard()
      on.exit(clipr::write_clip(old), add = TRUE)
      force(code)
    })()
  )
}

test_clipboard <- function(x, ...) {
  with_clip({
    expect_error(write_clipboard(x), NA)
    expect_equal(read_clipboard(), x, ...)
  })
}

expect_clip <- function(input, method) {
  with_clip({
    write_clipboard(input)
    res <- if (package_available("tibble")) {
      tibble::tibble(a = 1L, b = 2L, c = 3L)
    } else {
      fuj::quick_dfl(a = 1L, b = 2L, c = 3L)
    }
    expect_identical(read_clipboard(method), res)
  })
}

simple_tbl <- function(delim) {
  paste(
    paste(letters[1:3], collapse = delim),
    paste(1:3, collapse = delim),
    sep = "\n"
  )
}

expect_deprecated <- function(expr) {
  expect_warning(expr, class = "deprecatedWarning")
}
