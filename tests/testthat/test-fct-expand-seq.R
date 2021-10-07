x <- ordered(letters[c(5:15, 2)], levels = letters)

test_that("Sequences correctly", {
  expect_equal(fct_expand_seq(x),
               ordered(letters[2:15], levels = letters))

  expect_equal(fct_expand_seq(x, "g", "s", 3L),
               ordered(letters[seq(7, 19, 3)], levels = letters))

  expect_equal(fct_expand_seq(x, "g", "s", 3L),
               fct_expand_seq(x, "g", "t", 3L))
  expect_equal(fct_expand_seq(x, 1),
               ordered(letters[1:15], levels = letters))
})

test_that("fct_expand_seq() fails", {
  expect_error(fct_expand_seq("a"))
  expect_error(fct_expand_seq(as.ordered("a"), by = NA))
  expect_error(fct_expand_seq(as.ordered("a"), min_lvl = NA))
  expect_error(fct_expand_seq(as.ordered("a"), max_lvl = NA))
})
