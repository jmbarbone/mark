test_that("remove_na()", {
  x <- c(1, 2, NA, 3, NaN)
  expect_equal(remove_na(x), c(1, 2, 3))
  expect_equal(remove_na(as.list(x)), list(1, 2, numeric(), 3, numeric()))
  expect_error(remove_na(data.frame(x = 1)))
})

test_that("remove_null()", {
  x <- list(a = 1, b = NULL, c = 1)
  expect_equal(remove_null(x), list(a = 1, c = 1))

  expect_error(remove_null(c(1, 2)))
  expect_error(remove_null(data.frame(x = NULL)))
})


test_that("na cols", {
  x <- data.frame(
    first = c(NA, 2, 3, 4),
    second = c(1, NA, 3, 4),
    all = not_available(length = 4),
    last = c(1, 2, 3, NA),
    all2 = not_available(length = 4),
    stringsAsFactors = FALSE
  )

  expect_equal(
    select_na_cols(x),
    x[c("all", "all2")]
  )

  expect_equal(
    remove_na_cols(x),
    x[c("first", "second", "last")]
  )

  expect_equal(
    is_na_cols(x),
    c(first = FALSE, second = FALSE, all = TRUE, last = FALSE, all2 = TRUE)
    )
})
