test_that("`%out%` returns opposite of `%in%`", {
  a <- 1:10
  b <- c(1, 3, 5, 9)
  expect_true(all(a %in% b != a %out% b))

  sstr <- c("c", "ab", "B", "bba", "c", NA, "@", "bla", "a", "Ba", "%")
  expect_true(
    no_match(
      sstr[sstr %in%  c(letters, LETTERS)],
      sstr[sstr %out% c(letters, LETTERS)])
  )
})

test_that("`%wo%` returns duplicates.", {
  expect_equal(
    c(1:6, 7:2) %wo% c(3, 7, 12),
    c(1, 2, 4, 5, 6, 6, 5, 4, 2)
  )
})
