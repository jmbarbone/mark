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
