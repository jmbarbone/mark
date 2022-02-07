
# fact.logical() ----

test_that("fact.logical() works", {
  x <- fact(c(TRUE, FALSE, NA))
  expect_message(capture.output(print(x)), NA)

  x <- fact(c(FALSE, NA, NA, TRUE, FALSE))
  res <- make_fact(c(2L, 3L, 3L, 1L, 2L), c(TRUE, FALSE, NA))
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
  res <- make_fact(c(1L, 3L, 3L, 4L, 1L, 2L), levels = c(0L, 5L, 10L, NA_integer_))

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
    make_fact(1L, levels = 1L)
  )
})

# fact.factor() ----

test_that("fact.factor() works", {
  # x <- fact(as.character(c(Sys.Date() + 5:1, NA))[sample(1:6, 20, TRUE)])

  x <- factor(letters)
  class(x) <- c("fact", "factor")
  expect_identical(fact(x), x)

  x <- ordered(x)
  class(x) <- c("fact", "ordered", "factor")
  expect_identical(fact(x), x)

  x <- struct(1L, c("fact", "factor"), levels = c("a", NA_character_))
  expect_identical(fact(x), x)

  # fact.fact() checks for NA and adds
  x <- factor(c(1, NA, 2))
  expect_identical(levels(fact(x)), c("1", "2", NA_character_))

  # moves NA to the end and reordered when number
  x <- factor(c(1, NA, 2), levels = c(2, NA, 1), exclude = NULL)
  res <- make_fact(c(1, 3, 2), levels = c(1, 2, NA))
  expect_identical(fact(x), res)

  x <- factor(c(NA, TRUE, FALSE))
  res <- make_fact(c(3L, 1L, 2L), levels = c(TRUE, FALSE, NA))
  expect_identical(fact(x), res)

  x <- factor(c(NA, TRUE, FALSE), exclude = NULL)
  res <- make_fact(c(3L, 1L, 2L), levels = c(TRUE, FALSE, NA))
  expect_identical(fact(x), res)
})

# fact.haven_labelled() ----

test_that("fact.haven_labelled() works", {
  skip_if_not_installed("haven")
  .haven_as_factor <- "haven" %colons% "as_factor.haven_labelled"
  haven_as_factor <- function(...) add_class(.haven_as_factor(...), "fact", 1L)

  # Integers
  r <- rep(1:3, 2)
  x <- haven::labelled(r, labels = c(good = 1, bad = 3))
  expect_identical(fact(x), haven_as_factor(x))

  x <- haven::labelled(r, labels = c(good = 1, bad = 3), label = "this")
  expect_identical(fact(x), haven_as_factor(x))

  x <- haven::labelled(r, labels = c(good = 1, neutral = 2, bad = 3), label = "this")
  expect_identical(fact(x), haven_as_factor(x))

  x <- haven::labelled(r, label = "this")
  expect_identical(fact(x), haven_as_factor(x))


  # Doubles
  x <- haven::labelled(c(0, 0.5, 1), c(a = 0, b = 1))
  expect_identical(fact(x), haven_as_factor(x))

  # Character
  x <- haven::labelled(letters, c(good = "j", something = "m", cool = "b"))
  expect_identical(fact(x), haven_as_factor(x))
})

# nas ----

test_that("fact() correctly labels NAs [#24]", {
  expect_equal(
    fact(c(NA, "a", "b")),
    make_fact(c(3L, 1L, 2L), c("a", "b", NA))
  )

  expect_equal(
    fact(c(NA, 1, 3)),
    make_fact(c(3L, 1L, 2L), c(1, 3, NA))
  )

  expect_equal(
    fact(c(1, NA, NA, 3)),
    make_fact(c(1L, 3L, 3L, 2L), c(1, 3, NA))
  )

  expect_equal(
    fact(c(TRUE, TRUE, NA, FALSE, TRUE)),
    make_fact(c(1L, 1L, 3L, 2L, 1L), c(TRUE, FALSE, NA))
  )
})


# ordering ----

test_that("as_ordered() works", {
  res <- fact(c(1:3, NA_integer_))
  exp <- struct(
    c(1:3, NA_integer_),
    c("fact", "ordered", "factor"),
    levels = as.character(1:3),
    uniques = 1:3
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
    uniques = 1:4
  )
  expect_identical(x, exp)
})


# fact_coerce_levels() ----------------------------------------------------

test_that("fact_coerce_levels() works", {
  x <- as.Date("2021-09-03") + 0:2
  expect_equal(fact_coerce_levels(as.character(x)), x)

  # Be careful about setting a time zone
  # Not very good for dealing with local
  x <- as.POSIXlt("2021-09-03", tz = "UTC") + 0:2
  expect_equal(fact_coerce_levels(as.character(x)), x)

  x <- as.POSIXlt("2021-09-03", tz = "UTC") + 0:2
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
  x <- as_ordered(factor(1, 1:2))
  exp <- struct(1L, class = c("fact", "ordered", "factor"), levels = "1", uniques = 1L)
  expect_equal(drop_levels(x), exp)
})



# other methods -----------------------------------------------------------

test_that("is.na.fact(), `is.na<-.fact`(), works", {
  x <- fact(c(1, 2, NA, 3))
  res <- is.na(x)
  exp <- c(FALSE, FALSE, TRUE, FALSE)
  expect_identical(res, exp)

  x <- fact(c("a", "b", "c"))
  res <- is.na(x)
  exp <- c(FALSE, FALSE, FALSE)
  expect_identical(res, exp)

  # debugonce(`is.na<-.fact`)

  nas <- c(TRUE, FALSE, FALSE)
  is.na(x) <- nas
  exp <- fact(c(NA, "b", "c"))

  expect_identical(x, exp)
  expect_identical(is.na(x), nas)
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
