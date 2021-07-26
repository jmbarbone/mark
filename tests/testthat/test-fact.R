test_that("fact.logical() works", {
  expect_message(capture.output(print(fact(c(TRUE, FALSE, NA)))), NA)
})

test_that("fact.pseudo_id() works", {
  expect_message(capture.output(print(fact(pseudo_id(c("a", "a", "b", NA_character_))))), NA)
})

test_that("fact.integer() works", {
  expect_equal(
    fact(struct(1L, c("foo", "integer"))),
    struct(1L, "factor", levels = "1")
  )
})

test_that("fact.haven_labelled() works", {
  skip_if_not_installed("haven")
  haven_as_factor <- "haven" %colons% "as_factor.haven_labelled"

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
    struct(c(3L, 1L, 2L), class = "factor", levels = c("a", "b", NA))
  )

  expect_equal(
    fact(c(NA, 1, 3)),
    struct(c(3L, 1L, 2L), class = "factor", levels = c("1", "3", NA))
  )

  expect_equal(
    fact(c(1, NA, NA, 3)),
    struct(c(1L, 3L, 3L, 2L), class = "factor", levels = c("1", "3", NA))
  )

  expect_equal(
    fact(c(TRUE, TRUE, NA, FALSE, TRUE)),
    struct(c(2L, 2L, 3L, 1L, 2L), class = "factor", levels = c("TRUE", "FALSE", NA))
  )
})
