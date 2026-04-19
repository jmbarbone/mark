test_that("md5() works", {
  expect_identical(
    md5(letters),
    "efdb4b76073d1962b351692972a8d9e3",
    ignore_attr = "class"
  )

  expect_identical(
    md5(1:100),
    "e7d409fa912dd31c9a9da6cb9eb21b32",
    ignore_attr = "class"
  )

  expect_identical(
    md5(quick_df(list(a = 1))),
    "f16311aa3f99060be519c8e9b38101e6",
    ignore_attr = "class"
  )
})

test_that("md5(btyes) return the same", {
  skip_if_not(getRversion() >= "4.5.0")
  expect_identical(
    md5(letters, bytes = TRUE),
    md5(letters, bytes = FALSE)
  )
})

test_that("snapshots", {
  expect_snapshot(md5(letters))
  expect_snapshot(md5(1:100))
  expect_snapshot(md5(dataframe(a = 1)))
})
