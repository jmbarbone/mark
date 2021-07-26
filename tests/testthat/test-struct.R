test_that("struct() works", {
  expect_equal(
    struct(1, "foo", a = 1),
    structure(1, class = "foo", a = 1)
  )

  # struct() doesn't apply class as an attribute
  expect_failure(
    expect_equal(
      struct("a", "character"),
      structure("a", class = "character")
    )
  )
})

test_that("struct() works for factors", {
  expect_error(
    struct(1, class = "factor", levels = "a"),
    'adding class "factor" to an invalid object'
  )

  expect_identical(
    struct(1L, class = "factor", levels = c("a", "b")),
    structure(1, class = "factor", levels = c("a", "b"))
  )
})

test_that("NULL handling", {
  expect_identical(struct(NULL, NULL), list())
  expect_identical(struct(NULL, "foo"), structure(list(), class = "foo"))
})

test_that("examples", {
  expect_warning(structure(NULL))
  expect_identical(wuffle(structure(NULL)), NULL)
  expect_identical(struct(NULL, NULL), list())

  x <- NULL
  attributes(x) <- NULL
  expect_null(x)
  attributes(x) <- list()
  expect_identical(x, list())
})
