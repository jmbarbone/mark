test_that("set_names() works", {
  expect_null(set_names(NULL))
})

test_that("names_switch() works", {
  x <- c(a = 1, b = 2, c = 3)
  y <- names(x)
  names(y) <- x

  expect_equal(names_switch(x), y)

  expect_equal(
    names_switch(list(a = 1:2, b = 2)),
    c(`1:2` = "a", `2` = "b")
  )
})

test_that("names_sort() works", {
  x <- set_names(rep(NA, 3), c(-1, 10, 2))

  expect_equal(
    sort_names(x),
    set_names(rep(NA, 3), c(-1, 10, 2))
  )

  expect_equal(
    sort_names(x, numeric = TRUE),
    set_names(rep(NA, 3), c(-1, 2, 10))
  )

  expect_error(sort_names(list(a = 1)), class = "sortByAtomicError")
  expect_error(sort_names(NA), class = "simpleError")
})

test_that("%names% works", {
  x <- 1:4
  nm <- letters[1:4]

  expect_identical(
    x %names% nm,
    set_names(x, nm)
  )
})
