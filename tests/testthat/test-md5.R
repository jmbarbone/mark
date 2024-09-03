test_that("md5() works", {
  expect_identical(
    md5(letters),
    "d50f072f51db46c1736ee43adbba195f",
    ignore_attr = "class"
  )

  expect_identical(
    md5(1:100),
    "9d6257190ff5b50a0242cea93afda52f",
    ignore_attr = "class"
  )

  expect_identical(
    md5(quick_dfl(a = 1)),
    "45aa38750405b63fd8cb81b938cdf76b",
    ignore_attr = "class"
  )
})

test_that("snapshots", {
  expect_snapshot(md5(letters))
  expect_snapshot(md5(1:100))
  expect_snapshot(md5(quick_dfl(a = 1)))
})
