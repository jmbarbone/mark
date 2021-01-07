test_that("handler examples", {
  expect_equal(
    has_warning(c(1, "no"), as.integer),
    c(`1` = FALSE, no = TRUE)
  )

  expect_equal(
    get_warning(c(1, "no"), as.integer),
    list(`1` = NULL, no = "NAs introduced by coercion")
  )

  expect_equal(
    get_warning(c(1, "no"), as.integer, .null = FALSE),
    list(no = "NAs introduced by coercion")
  )

  foo <- function(x) {
    stopifnot(x > 0)
    x
  }

  expect_equal(
    has_error(c(1, 0, 2), foo),
    c(`1` = FALSE, `0` = TRUE, `2` = FALSE)
  )

  expect_equal(
    get_error(c(1, 0, 2), foo),
     list(`1` = NULL, `0` = "x > 0 is not TRUE", `2` = NULL)
  )

  expect_equal(
    get_error(c(1, 0, 2), foo, .null = FALSE),
    list(`0` = "x > 0 is not TRUE")
  )
})
