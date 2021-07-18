test_that("fact.logical() works", {
  expect_message(capture.output(print(fact(c(TRUE, FALSE, NA)))), NA)
})

test_that("fact.pseudo_id() works", {
  expect_message(capture.output(print(fact(pseudo_id(c("a", "a", "b", NA_character_))))), NA)
})

test_that("fact() correctly labels NAs [#24]", {
  expect_equal(
    fact(c(NA, "a", "b")),
    structure(c(3L, 1L, 2L), class = "factor", levels = c("a", "b", NA))
  )

  expect_equal(
    fact(c(NA, 1, 3)),
    structure(c(3, 1, 2), class = "factor", levels = c("1", "3", NA))
  )

  expect_equal(
    fact(c(1, NA, NA, 3)),
    structure(c(1, 3, 3, 2), class = "factor", levels = c("1", "3", NA))
  )

  expect_equal(
    fact(c(TRUE, TRUE, NA, FALSE, TRUE)),
    structure(c(2, 2, 3, 1, 2), class = "factor", levels = c("TRUE", "FALSE", NA))
  )
})
