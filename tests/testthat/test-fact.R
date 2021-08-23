test_that("fact.logical() works", {
  x <- fact(c(TRUE, FALSE, NA))
  expect_message(capture.output(print(x)), NA)

  x <- fact(c(FALSE, NA, NA, TRUE, FALSE))
  res <- make_fact(c(2L, 3L, 3L, 1L, 2L), c("TRUE", "FALSE", NA_character_))
  expect_identical(x, res)

  expect_false(anyNA(x))
})

test_that("fact.pseudo_id() works", {
  expect_message(capture.output(print(fact(pseudo_id(c("a", "a", "b", NA_character_))))), NA)

  # Should appropriately order numeric values
  x <- c(0, 10, 10, NA, 0, 5)
  id <- pseudo_id(x)
  f <- fact(id)
  res <- make_fact(c(1L, 3L, 3L, 4L, 1L, 2L), levels = c("0", "5", "10", NA))

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

test_that("fact.integer() works", {
  expect_equal(
    fact(struct(1L, c("foo", "integer"))),
    make_fact(1L, levels = "1")
  )
})

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
  res <- make_fact(c(1L, 3L, 2L), levels = c("1", "2", NA_character_))
  expect_identical(fact(x), res)
})

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

test_that("fact() correctly labels NAs [#24]", {
  expect_equal(
    fact(c(NA, "a", "b")),
    make_fact(c(3L, 1L, 2L), c("a", "b", NA))
  )

  expect_equal(
    fact(c(NA, 1, 3)),
    make_fact(c(3L, 1L, 2L), c("1", "3", NA))
  )

  expect_equal(
    fact(c(1, NA, NA, 3)),
    make_fact(c(1L, 3L, 3L, 2L), c("1", "3", NA))
  )

  expect_equal(
    fact(c(TRUE, TRUE, NA, FALSE, TRUE)),
    make_fact(c(1L, 1L, 3L, 2L, 1L), c("TRUE", "FALSE", NA))
  )
})
