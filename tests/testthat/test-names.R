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
  x <- set_names0(rep(NA, 3), c(-1, 10, 2))

  expect_equal(
    sort_names(x),
    set_names0(rep(NA, 3), c(-1, 10, 2))
  )

  expect_equal(
    sort_names(x, numeric = TRUE),
    set_names0(rep(NA, 3), c(-1, 2, 10))
  )

  expect_error(sort_names(list(a = 1)))
  expect_error(sort_names(NA))
})
