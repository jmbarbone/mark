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


test_that("*_na_cols() works", {
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

  expect_error(select_na_cols(1))
  expect_error(remove_na_cols(1))
  expect_error(is_na_cols(1))
})


test_that("tableNA() works", {
  x <- list(
    a = c(1, 2, NA, 3),
    b = c("A", NA, "B", "C"),
    c = as.Date(c("2020-01-02", NA, NA, "2020-03-02"))
  )

  expect_identical(
    tableNA(x),
    struct(
      c(`TRUE` = 0L, `FALSE` = 3L),
      dim = 2L,
      dimnames = set_names0(list(c("TRUE", "FALSE")), ""),
      class = "table"
    )
  )

  expect_identical(
    tableNA(x, .list = TRUE),
    struct(
      c(0L, 1L, 1L, 0L, 0L, 0L, 0L, 2L),
      dim = c(2L, 2L, 2L),
      dimnames = list(
        a = c("TRUE", "FALSE"),
        b = c("TRUE", "FALSE"),
        c = c("TRUE", "FALSE")
      ),
      class = "table"
    )
  )

  x <- y <- c(NA, 1)

  expect_identical(
    tableNA(x, y),
    struct(
      c(1L, 0L, 0L, 1L),
      dim = c(2L, 2L),
      dimnames = list(x = c("TRUE", "FALSE"), y = c("TRUE", "FALSE")),
      class = "table"
    )
  )
})
