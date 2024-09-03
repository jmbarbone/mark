

# counts() ----------------------------------------------------------------

test_that("counts.default() work", {
  x <- rep(c("a", "b", "c"), c(3, 1, 4))
  res <- set_names(c(3, 1, 4), c("a", "b", "c"))
  ans <- counts(x)
  expect_equal(ans, res)

  expect_equal(props(x), ans / sum(ans))

  x <- rep(c(2, 3, -1), c(3, 1, 4))
  res <- set_names(c(3, 1, 4), c(2, 3, -1))
  expect_equal(counts(x), res)

  res <- sort_names(res)
  expect_equal(counts(x, sort = TRUE), res)
})

test_that("counts() works with NAs", {

  # Correct sort
  x <- c(FALSE, TRUE, NA)
  res <- set_names(c(1, 1, 1), x)
  expect_equal(counts(x), res)

  x <- c("false", "true", NA_character_)
  res <- set_names(c(1, 1, 1), x)
  expect_equal(counts(x), res)

  x <- c(1L, 0L, NA_integer_, NA_integer_)
  res <- set_names(c(1L, 1L, 2L), c(1, 0, NA))
  expect_equal(counts(x), res)
})


test_that("counts.data.frame() adds new name", {
  df <- quick_dfl(a = 1, b = 2)
  res <- counts(df, 1)
  expect_equal(colnames(res), c("a", "freq"))

  # Default to "freq"
  res <- counts(df, 1, .name = NULL)
  expect_equal(colnames(res), c("a", "freq"))

  res <- counts(df, 1, .name = "new_name")
  expect_equal(colnames(res), c("a", "new_name"))

  # Default to "prop"
  res <- props(df, 1, .name = NULL)
  expect_equal(colnames(res), c("a", "prop"))

  res <- props(df, 1)
  expect_equal(colnames(res), c("a", "prop"))

  res <- props(df, 1, .name = "new_name")
  expect_equal(colnames(res), c("a", "new_name"))
})

test_that("counts() NAs are last", {
  expect_equal(
    counts(c(NA, NA, 1, 2)),
    set_names(c(1, 1, 2), c(1, 2, NA))
  )

  expect_equal(
    counts(c(NA, NA, 1)),
    set_names(c(1, 2), c(1, NA))
  )

  expect_equal(
    counts(c("a", NA, NA)),
    set_names(c(1, 2), c("a", NA))
  )

  expect_equal(
    counts(c(NA_real_, NA_real_)),
    set_names(2, NA)
  )
})

test_that("counts.data.frame() works", {
  df <- quick_dfl(a = rep("x", 3), b = fact(c("a", "a", "b")))

  res <- counts(df, 1)
  exp <- quick_dfl(a = "x", freq = 3)
  expect_equal(res, exp)

  res <- counts(df, 2)
  exp <- quick_dfl(b = fact(c("a", "b")), freq = 2:1)
  expect_equal(res, exp)

  res <- counts(df, 1:2)
  exp <- quick_dfl(a = rep("x", 2), b = fact(c("a", "b")), freq = 2:1)
  expect_equal(res, exp)

  df[["b"]] <- as_ordered(df[["b"]])
  res <- counts(df, 1:2)
  exp <- quick_dfl(a = rep("x", 2), b = as_ordered(c("a", "b")), freq = 2:1)
  expect_equal(res, exp)
})

test_that("missing upper levels", {
  x <- struct(1:2, class = "factor", levels = c(letters[1:4]))
  exp <- c(a = 1L, b = 1L, c = 0L, d = 0L)
  expect_equal(counts(x), exp)
})


# props() -----------------------------------------------------------------

test_that("props() handles NA", {
  x <- c(1, 1, 2, NA, 4)
  res1 <- set_names(c(.4, .2, .2, .2), c(1, 2, 4, NA))
  res2 <- set_names(c(.5, .25, .25, NA), c(1, 2, 4, NA))
  expect_identical(props(x), res1)
  expect_identical(props(x, na.rm = TRUE), res2)

  # data frame
  x <- c(1, 2, 2, 3, NA)
  y <- flip(x)
  df <- quick_dfl(x = x, y = y)

  res_x1 <- quick_dfl(x = c(1, 2, 3, NA), prop = c(.20, .40, .20, .20))
  res_x2 <- quick_dfl(x = c(1, 2, 3, NA), prop = c(.25, .50, .25,  NA))
  expect_identical(props(df, "x"), res_x1)
  expect_identical(props(df, "x", na.rm = TRUE), res_x2)

  res_xy1 <- quick_dfl(
    x = c(1, 2, 2, 3, NA),
    y = c(NA, 3, 2, 2, 1),
    prop = rep(.2, 5)
  )
  res_xy2 <- quick_dfl(
    x = c(1, 2, 2, 3, NA),
    y = c(NA, 3, 2, 2, 1),
    prop = c(NA, 1, 1, 1, NA) / 3
  )

  expect_identical(props(df, c("x", "y")), res_xy1)
  expect_identical(props(df, c("x", "y"), na.rm = TRUE), res_xy2)
})
