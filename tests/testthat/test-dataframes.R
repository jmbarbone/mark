test_that("to_row_names()", {
  x <- data.frame(
   a = 1:4,
   b = letters[1:4]
  )

  expect_equal(
    to_row_names(x),
    data.frame(
      b = letters[1:4]
    )
  )

  expect_equal(
    to_row_names(x, "b"),
    data.frame(
      a = 1:4,
      row.names = letters[1:4]
    )
  )

  expect_equal(
    to_row_names(x, 2L),
    to_row_names(x, "b")
  )

  # non-integers to character
  foo <- function(x) {
    out <- to_row_names(data.frame(a = 1, b = x), "b")
    class(attr(out, "row.names"))
  }

  expect_equal(foo(-1L), "integer")
  expect_equal(foo(1.3), "character")
  expect_equal(foo(Sys.Date()), "character")
})


test_that("vector2df()", {
  x <- c(1.0, 3.1, 8.2)
  df <- data.frame(name = character(3),
                    value = x)
  expect_equal(vector2df(x), df)
  df$name <- as.character(x)
  expect_equal(vector2df(set_names(x)), df)
  expect_named(vector2df(x, "one", "two"), c("one", "two"))

  df <- data.frame(name = rep(NA_character_, 3),
                   value = x)
  expect_equal(vector2df(x, show_NA = TRUE), df)
})

test_that("list2df()", {
  x <- list(a = 1, b = 2:4, c = letters[10:20])
  exp <- data.frame(name = letters[c(1, rep(2, 3), rep(3, 11))],
                    value = c(1, 2:4, letters[10:20]))

  expect_warning(expect_warning(list2df(x)))
  expect_warning(list2df(x, warn = FALSE), NA)
  expect_equal(list2df(x, warn = FALSE), exp)

  x <- list(a = 1, b = seq(2, 9, 1.0), c = 20)
  exp <- data.frame(name = c("a", rep("b", 8), "c"),
                    value = c(1:9, 20))
  expect_warning(list2df(x), NA)
  expect_equal(list2df(x), exp)
  expect_named(list2df(x, "hello", "world"), c("hello", "world"))
})

test_that("t_df()", {
  x <- data.frame(
    a = 1:5,
    b = letters[1:5]
  )

  y <- data.frame(
    colname = c("a", "b"),
    row_1 = c(1,   "a"),
    row_2 = c(2,   "b"),
    row_3 = c(3,   "c"),
    row_4 = c(4,   "d"),
    row_5 = c(5,   "e")
  )

  expect_equal(t_df(x), y)
})
