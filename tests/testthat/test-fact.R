
# fact.logical() ----

test_that("fact.logical() works", {
  x <- fact(c(TRUE, FALSE, NA))
  expect_message(capture.output(print(x)), NA)

  x <- fact(c(FALSE, NA, NA, TRUE, FALSE))
  res <- new_fact(c(2L, 3L, 3L, 1L, 2L), c(TRUE, FALSE, NA))
  expect_identical(x, res)

  expect_false(anyNA(x))
})

# fact.pseudo_id() ----

test_that("fact.pseudo_id() works", {
  expect_message(capture.output(print(fact(pseudo_id(c("a", "a", "b", NA_character_))))), NA)

  # Should appropriately order numeric values
  x <- c(0L, 10L, 10L, NA, 0L, 5L)
  id <- pseudo_id(x)
  f <- fact(id)
  res <- new_fact(c(1L, 3L, 3L, 4L, 1L, 2L), c(0L, 5L, 10L, NA_integer_))

  expect_identical(fact(x), f)
  expect_identical(fact(x), res)

  # Shouldn't have any NA
  expect_false(anyNA(fact(x)))

  # pseudo_id() ordered by appearance, fact() has pre-set ordering
  expect_identical(pseudo_id(f), id)

  # attr(id, "uniques") <- as.character(attr(id, "uniques"))
  # factors store levels as characters
  o <- order(as.integer(levels(f)))
  expect_identical(o, 1:4)
})

# fact.integer() ----

test_that("fact.integer() works", {
  expect_equal(
    fact(struct(1L, c("foo", "integer"))),
    new_fact(1L, levels = 1L)
  )
})

# fact.factor() ----

test_that("fact.factor() works", {
  # x <- fact(as.character(c(Sys.Date() + 5:1, NA))[sample(1:6, 20, TRUE)])

  x <- factor(letters)
  class(x) <- c("fact", "factor")
  expect_identical(fact(x), x)

  x <- ordered(letters)
  class(x) <- c("fact", "ordered", "factor")
  expect_identical(fact(x), x)

  x <- struct(1L, c("fact", "factor"), levels = c("a", NA_character_))
  expect_identical(fact(x), x)

  # fact.fact() checks for NA and adds
  x <- factor(c(1, NA, 2))
  expect_identical(levels(fact(x)), c("1", "2", NA_character_))

  # moves NA to the end and reordered when number
  x <- factor(c(1, NA, 2), levels = c(2, NA, 1), exclude = NULL)
  res <- new_fact(c(1, 3, 2), levels = c(1, 2, NA))
  expect_identical(fact(x), res)

  x <- factor(c(NA, TRUE, FALSE))
  res <- new_fact(c(3L, 1L, 2L), levels = c(TRUE, FALSE, NA))
  expect_identical(fact(x), res)

  x <- factor(c(NA, TRUE, FALSE), exclude = NULL)
  res <- new_fact(c(3L, 1L, 2L), levels = c(TRUE, FALSE, NA))
  expect_identical(fact(x), res)
})

# fact.haven_labelled() ----

test_that("fact.haven_labelled() works", {
  skip_if_not_installed("haven")
  .haven_as_factor <- "haven" %colons% "as_factor.haven_labelled"
  haven_as_fact <- function(...) {
    res <- fact(.haven_as_factor(...))
    attr(res, "label") <- exattr(..1, "label")
    res
  }

  expect_id_fact <- function(x) {
    testthat::expect_identical(
      fact(x),
      haven_as_fact(x),
      ignore_attr = c("uniques", "na")
    )
  }

  # Integers
  r <- rep(1:3, 2)
  x <- haven::labelled(r, labels = c(good = 1, bad = 3))
  expect_id_fact(x)

  x <- haven::labelled(r, labels = c(good = 1, bad = 3), label = "this")
  expect_id_fact(x)

  x <- haven::labelled(r, labels = c(good = 1, neutral = 2, bad = 3), label = "this")
  expect_id_fact(x)

  x <- haven::labelled(r, label = "this")
  expect_id_fact(x)

  # Doubles
  x <- haven::labelled(c(0, 0.5, 1), c(a = 0, b = 1))
  expect_id_fact(x)

  # Character
  x <- haven::labelled(letters, c(good = "j", something = "m", cool = "b"))
  expect_id_fact(x)

  # Unique not in levels; levels not in unique
  x <- haven::labelled(
    c(-10, 20, 40, 60),
    labels = c(a = 10, b = 20, c = 30, d = 40),
    label = "foo"
  )
  expect_id_fact(x)
})

# nas ----

test_that("fact() correctly labels NAs [#24]", {
  expect_equal(
    fact(c(NA, "a", "b")),
    new_fact(c(3L, 1L, 2L), c("a", "b", NA))
  )

  expect_equal(
    fact(c(NA, 1, 3)),
    new_fact(c(3L, 1L, 2L), c(1, 3, NA))
  )

  expect_equal(
    fact(c(1, NA, NA, 3)),
    new_fact(c(1L, 3L, 3L, 2L), c(1, 3, NA))
  )

  expect_equal(
    fact(c(TRUE, TRUE, NA, FALSE, TRUE)),
    new_fact(c(1L, 1L, 3L, 2L, 1L), c(TRUE, FALSE, NA))
  )
})


test_that("fact() ignores NaN", {
  # ignore NaN
  res <- fact(c(1, 2, NA, 3, NaN))
  exp <- struct(
    c(1L, 2L, 4L, 3L, 4L),
    class = c("fact", "factor"),
    levels = c("1", "2", "3", NA),
    uniques = c(1, 2, 3, NA),
    na = 4L
  )

  expect_identical(res, exp)
})


# ordering ----

test_that("as_ordered() works", {
  res <- fact(c(1:3, NA_integer_))
  exp <- struct(
    c(1:3, NA_integer_),
    c("fact", "ordered", "factor"),
    levels = as.character(1:3),
    uniques = 1:3,
    na = 0L
  )
  expect_identical(as_ordered(res), exp)
})

test_that("as_ordered() doesn't duplicate class", {
  res <- class(as_ordered(as.ordered(letters[1:3])))
  expect_identical(res, c("fact", "ordered", "factor"))
})


# fact.default() ----------------------------------------------------------

test_that("fact.default() fails", {
  expect_error(fact(struct(NULL, "foo")))
})


# `fact_levels<-`() -------------------------------------------------------

test_that("`fact_levels<-`() works", {
  x <- fact(1:3)
  fact_levels(x) <- 1:4
  exp <- struct(
    1:3,
    class = c("fact", "factor"),
    levels = as.character(1:4),
    uniques = 1:4,
    na = 0L
  )
  expect_identical(x, exp)
})


# fact_coerce_levels() ----------------------------------------------------

test_that("fact_coerce_levels() works", {
  x <- as.Date("2021-09-03") + 0:2
  expect_equal(fact_coerce_levels(as.character(x)), x)

  # Be careful about setting a time zone
  # Not very good for dealing with local
  # NOTE r-dev after 4.2.1 has some weird behavior with the 0 and returns:
  #  ('2021-09-03', '2021-09-03 00:00:01', '2021-09-03 00:00:02')
  x <- as.POSIXlt("2021-09-03", tz = "UTC") + 1:3
  expect_equal(fact_coerce_levels(as.character(x)), x)

  x <- as.POSIXlt("2021-09-03", tz = "UTC") + 1:3
  expect_equal(fact_coerce_levels(as.character(x)), x)
})


# try_numeric() -----------------------------------------------------------

test_that("try_numeric() works", {
  x <- 1
  expect_identical(try_numeric(x), x)

  x <- c(NA, NA)
  expect_identical(try_numeric(x), x)

  x <- c("a", 1)
  expect_identical(try_numeric(x), x)

  x <- c(1, NA, 2)
  expect_identical(try_numeric(as.character(x)), x)
})


# drop_levels -------------------------------------------------------------

test_that("drop_levels() works", {
  x <- factor(1, 1:2)
  exp <- factor(1, 1)
  expect_equal(drop_levels(x), exp)

  df <- quick_dfl(x = x, y = 1)
  df_exp <- quick_dfl(x = exp, y = 1)
  expect_equal(drop_levels(df), df_exp)

  # facts and ordered
  x <- fact(1:10)
  expect_identical(drop_levels(x), x)

  x <- as_ordered(factor(1, 1:2))
  exp <- struct(1L, class = c("fact", "ordered", "factor"), levels = "1", uniques = 1L, na = 0L)
  expect_equal(drop_levels(x), exp)
})


# fact_reverse() ----------------------------------------------------------

test_that("fact_reverse() works", {
  res <- fact_reverse(1:4)
  exp <- new_fact(4:1, 4:1)
  expect_identical(res, exp)

  res <- fact_reverse(as_ordered(1:4))
  exp <- new_fact(4:1, 4:1, ordered = TRUE)
  expect_identical(res, exp)

  res <- fact_reverse(c(1:3, NA))
  exp <- new_fact(c(3:1, 4L), c(3:1, NA))
  expect_identical(res, exp)

  res <- fact_reverse(as_ordered(c(1:3, NA)))
  exp <- struct(
    c(3:1, NA),
    class = c("fact", "ordered", "factor"),
    levels = c("3", "2", "1"),
    uniques = 3:1,
    na = 0L
  )

  expect_identical(res, exp)
})



# other methods -----------------------------------------------------------

test_that("[.fact() works", {
  x <- fact(1:3)
  x1 <- do.call(structure, c(1, attributes(x)))
  x2 <- do.call(structure, c(2, attributes(x)))
  expect_identical(x[1], x1)
  expect_identical(x[2], x2)
})

test_that("is.na.fact(), works", {
  x <- fact(c(1, 2, NA, 3))
  res <- is.na(x)
  exp <- c(FALSE, FALSE, FALSE, FALSE)
  expect_identical(res, exp)

  x <- fact_na(c(1, 2, NA, 3))
  res <- is.na(x)
  exp <- c(FALSE, FALSE, TRUE, FALSE)
  expect_identical(res, exp)

  x <- fact_na(c("a", "b", "c"))
  res <- is.na(x)
  exp <- c(FALSE, FALSE, FALSE)
  expect_identical(res, exp)
})

test_that("as.integer.fact() works", {
  x <- fact(c(1, 2, NA, 3))
  res <- as.integer(x)
  exp <- c(1L, 2L, NA_integer_, 3L)
  expect_identical(res, exp)
})

test_that("as.integer.fact() works", {
  x <- fact(c(1, 2, NA, 3))
  res <- as.double(x)
  exp <- c(1, 2, NA_real_, 3)
  expect_identical(res, exp)
  expect_identical(as.numeric(x), exp)
})

test_that("unique.fact() works", {
  x <- fact(c(1, 2, NA, 3, 2))
  exp <- fact(c(1, 2, NA, 3))
  res <- unique(x)
  expect_identical(exp, res)

  x <- as_ordered(x)
  exp <- as_ordered(exp)
  res <- unique(x)
  expect_identical(exp, res)
})

test_that("as.Date.fact() works", {
  exp <- as.Date(c("2022-01-02", NA, "1908-12-21"))
  res <- as.Date(fact(exp))
  expect_identical(exp, res)

  x <- c("01-01-2022", "01-02-2000")
  exp <- as.Date(x, "%d-%m-%Y")
  res <- as.Date(fact(x), "%d-%m-%Y")
  expect_identical(exp, res)
})

test_that("as.character.fact() works", {
  exp <- c("a", NA_character_, "b", "b", "a")
  res <- as.character(fact(exp))
  expect_identical(exp, res)
})

# snapshots ---------------------------------------------------------------

test_that("snapshots", {
  expect_snapshot(fact(character()))
  expect_snapshot(fact(1:5))
  expect_snapshot(print(fact(1:100), max_levels = TRUE))
  expect_snapshot(print(fact(1:100), max_levels = 20))
  expect_snapshot(print(fact(1:100), max_levels = 100))
})
