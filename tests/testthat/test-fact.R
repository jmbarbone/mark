test_that("fact.logical() works", {
  expect_message(capture.output(print(fact(c(TRUE, FALSE, NA)))), NA)
})

test_that("fact.pseudo_id() works", {
  expect_message(capture.output(print(fact(pseudo_id(c("a", "a", "b", NA_character_))))), NA)
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
