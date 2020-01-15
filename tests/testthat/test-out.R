context("Testing out functionality")

test_that("`%out%` returns opposite of `%in%`", {

    # examples from `%in%`
  expect_true(opposite(1:10 %in%  c(1,3,5,9),
                       1:10 %out% c(1,3,5,9)))

  sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","%")
  expect_true(no_match(sstr[sstr %in%  c(letters, LETTERS)],
                       sstr[sstr %out% c(letters, LETTERS)]))

})

test_that("`%w/o%` returns duplicates.", {
  expect_equal(c(1:6,7:2) %w/o% c(3,7,12),
               c(1, 2, 4, 5, 6, 6, 5, 4, 2))
})

test_that("all_na() returns correctly from example,", {
  expect_true(all_na(c(NA, NaN, NA_character_, NA_complex_)))
})
