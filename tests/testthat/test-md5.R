test_that("md5() works", {
  expect_identical(
    md5(letters),
    "b7fdd99fac291c4bbf958d9aee731951",
    ignore_attr = "class"
  )
})

test_that("snapshopts", {
  expect_snapshot(md5(data.frame(a = 1)))
})
