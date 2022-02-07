test_that("to_row_names()", {
  x <- quick_dfl(a = 1:4, b = letters[1:4])

  expect_equal(
    to_row_names(x),
    quick_dfl(b = letters[1:4])
  )

  expect_equal(
    to_row_names(x, "b"),
    data.frame(a = 1:4, row.names = letters[1:4])
  )

  expect_equal(
    to_row_names(x, 2L),
    to_row_names(x, "b")
  )

  # non-integers to character
  foo <- function(x) {
    out <- to_row_names(quick_dfl(a = 1, b = x), "b")
    class(attr(out, "row.names"))
  }

  expect_equal(foo(-1L), "integer")
  expect_equal(foo(1.3), "character")
  expect_equal(foo(Sys.Date()), "character")
})


test_that("col_to_rn()", {
  expect_error(col_to_rn(data.frame(), 1:2), "must be a single element")
  expect_error(col_to_rn(data.frame(), NA), "is invalid")
})

test_that("vector2df()", {
  x <- c(1.0, 3.1, 8.2)
  df <- quick_dfl(name = c(NA, NA, NA), value = x)

  expect_equal(vector2df(x), df)
  df$name <- as.character(x)
  expect_equal(vector2df(set_names0(x)), df)
  expect_named(vector2df(x, "one", "two"), c("one", "two"))

  expect_warning(vector2df(x, show_NA = NULL), info = "show_NA should not be set")
  expect_error(vector2df(list(a = 1)), "non-list vector")
})

test_that("list2df()", {
  x <- list(a = 1, b = 2:4, c = letters[10:20])
  exp <- quick_dfl(
    name = letters[c(1, rep(2, 3), rep(3, 11))],
    value = c(1, 2:4, letters[10:20])
  )

  expect_warning(list2df(x))
  expect_warning(list2df(x, warn = FALSE), NA)
  expect_equal(list2df(x, warn = FALSE), exp)

  x <- list(a = 1, b = seq(2, 9, 1.0), c = 20)
  exp <- quick_df(list(name = c("a", rep("b", 8), "c"),
                       value = c(1:9, 20)))
  expect_warning(list2df(x), NA)
  expect_equal(list2df(x), exp)
  expect_named(list2df(x, "hello", "world"), c("hello", "world"))
  expect_warning(list2df(x, show_NA = NULL), info = "show_NA should not be set")

  # Unnamed
  x <- list(a = 1, 0, 2)
  res <- quick_dfl(name = c("a", 2, 3), value = c(1, 0, 2))
  expect_equal(list2df(x), res)

  expect_error(list2df(1), "must be a list")

  # Not sure this will continue to be the case
  expect_identical(quick_df(NULL), data.frame())
})

test_that("list2df2()", {
  res <- list2df2(list())
  exp <- structure(list(), names = character(0), class = "data.frame", row.names = integer())
  expect_identical(res, exp)
})

test_that("t_df()", {
  x <- quick_dfl(a = 1:5, b = letters[1:5])

  y <- quick_dfl(
    colname = c("a", "b"),
    row_1   = c(  1, "a"),
    row_2   = c(  2, "b"),
    row_3   = c(  3, "c"),
    row_4   = c(  4, "d"),
    row_5   = c(  5, "e")
  )

  expect_equal(t_df(x), y)
  expect_warning(t_df(x, id = 1))
  expect_error(t_df(1L))
})

test_that("quick_df()", {
  expect_error(quick_df(1L))
  expect_error(quick_df(list(a = 1:2, b = 1:3)))

  expect_identical(
    quick_df(list(a = NULL)),
    struct(list(a = NULL), "data.frame", names = "a", row.names = integer())
  )

  expect_identical(
    quick_df(list(a = integer())),
    data.frame(a = integer(), stringsAsFactors = FALSE)
  )

  expect_equal(
    quick_df(list(a = integer())),
    quick_dfl(a = integer())
  )
})

test_that("rn_to_col()", {
  # additional tests
  expect_error(rn_to_col(1L))
})

test_that("complete_cases()", {
  expect_error(complete_cases(1L))
  expect_error(complete_cases(quick_df(NULL)))

  df <- quick_dfl(a = c(1, 2, NA, 4), b = c(NA, 2, 3, NA))
  res1 <- quick_dfl(a = c(1, 2, 4), b = c(NA, 2, NA))
  res2 <- quick_dfl(a = 2, b = 2)

  expect_identical(complete_cases(df, "a"), res1)
  expect_identical(complete_cases(df), res2)
})
