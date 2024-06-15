test_that("md5() works", {
  expect_identical(
    md5(letters),
    "bc37452c4ba81b8d2bbb3aa6cf204c32",
    ignore_attr = "class"
  )

  expect_identical(
    md5(1:100),
    "0098c968f60e9718b250297103b5388b",
    ignore_attr = "class"
  )

  expect_identical(
    md5(quick_dfl(a = 1)),
    "6311f1f7467adb01d7d97447f650b452",
    ignore_attr = "class"
  )
})

test_that("snapshopts", {
  expect_snapshot(md5(letters))
  expect_snapshot(md5(1:100))
  expect_snapshot(md5(quick_dfl(a = 1)))
})
