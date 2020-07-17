context("vector and lists to data.frame")

test_that("vector to data.frame", {
  x <- c(1.0, 3.1, 8.2)
  df <- data.frame(name = character(3),
                    value = x)
  expect_equal(vector2df(x), df)
  df$name <- as.character(x)
  expect_equal(vector2df(setNames(nm = x)), df)
  expect_named(vector2df(x, "one", "two"), c("one", "two"))

  df <- data.frame(name = rep(NA_character_, 3),
                   value = x)
  expect_equal(vector2df(x, show_NA = TRUE), df)
})

test_that("list to data.frame", {
  x <- list(a = 1, b = 2:4, c = letters[10:20])
  exp <- data.frame(name = letters[c(1, rep(2, 3), rep(3, 11))],
                    value = c(1, 2:4, letters[10:20]))

  expect_warning(list2df(x))
  expect_warning(list2df(x, warn = FALSE), NA)
  expect_equal(list2df(x, warn = FALSE), exp)

  x <- list(a = 1, b = seq(2, 9, 1.0), c = 20)
  exp <- data.frame(name = c("a", rep("b", 8), "c"),
                    value = c(1:9, 20))
  expect_warning(list2df(x), NA)
  expect_equal(list2df(x), exp)
  expect_named(list2df(x, "hello", "world"), c("hello", "world"))
})
