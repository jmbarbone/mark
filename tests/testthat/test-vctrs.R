test_that("vctrs doesn't complain", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("vctrs")
  df <- data.frame(a = 1:4, b = c(1, 1, 2, 2))

  a <- pseudo_id(1:3)
  b <- pseudo_id(c(1L, 1L, 2L, 2L))
  # debugonce(c)
  expect_error(c(a, b), NA)
  pid <- new_pseudo_id()
  expect_identical(vctrs::vec_ptype_abbr(pid), "psid")

  vctrs::vec_ptype2(a, b)
  dplyr::mutate(df, pseudo_id(a))
  dplyr::mutate(dplyr::group_by(df, a), c = pseudo_id(b))
})
