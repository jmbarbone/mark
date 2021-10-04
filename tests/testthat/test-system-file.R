test_that("make_sf() ~works~ doesn't fail", {
  expect_type(make_sf("foo"), "closure")
})
