test_that("make_sf() ~works~ doesn't fail", {
  expect_type(sf0 <- make_sf("foo"), "closure")
  expect_identical(sf0, sf, ignore_function_env = TRUE)
  expect_identical(sf0(23232), "")
  expect_error(sf0(23232, check = TRUE))
})
