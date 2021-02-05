test_that("to_row_names()", {
  x <- quick_df(list(
   a = 1:4,
   b = letters[1:4]
  ))

  expect_equal(
    to_row_names(x),
    quick_df(list(
      b = letters[1:4]
    ))
  )

  expect_equal(
    to_row_names(x, "b"),
    data.frame(
      a = 1:4,
      row.names = letters[1:4],
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    to_row_names(x, 2L),
    to_row_names(x, "b")
  )

  # non-integers to character
  foo <- function(x) {
    out <- to_row_names(quick_df(list(a = 1, b = x)), "b")
    class(attr(out, "row.names"))
  }

  expect_equal(foo(-1L), "integer")
  expect_equal(foo(1.3), "character")
  expect_equal(foo(Sys.Date()), "character")
})


test_that("vector2df()", {
  x <- c(1.0, 3.1, 8.2)
  df <- quick_df(list(
    name = c(NA, NA, NA),
    value = x
  ))

  expect_equal(vector2df(x), df)
  df$name <- as.character(x)
  expect_equal(vector2df(set_names0(x)), df)
  expect_named(vector2df(x, "one", "two"), c("one", "two"))

  expect_warning(vector2df(x, show_NA = NULL), info = "show_NA should not be set")
})

test_that("list2df()", {
  x <- list(a = 1, b = 2:4, c = letters[10:20])
  exp <- quick_df(list(
    name = letters[c(1, rep(2, 3), rep(3, 11))],
    value = c(1, 2:4, letters[10:20])
  ))

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
  res <- quick_df(list(
    name = c("a", 2, 3),
    value = c(1, 0, 2)
  ))
  expect_equal(list2df(x), res)
})

test_that("t_df()", {
  x <- quick_df(list(
    a = 1:5,
    b = letters[1:5]
  ))

  y <- quick_df(list(
    colname = c("a", "b"),
    row_1   = c(  1, "a"),
    row_2   = c(  2, "b"),
    row_3   = c(  3, "c"),
    row_4   = c(  4, "d"),
    row_5   = c(  5, "e")
  ))

  expect_equal(t_df(x), y)
})
