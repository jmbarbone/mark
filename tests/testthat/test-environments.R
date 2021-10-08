test_that("environments() and friends works", {
  expect_error(environments(), NA)
  expect_snapshot(environments())

  expect_true(all(vap_lgl(unlist(ls_all()), exists)))
  expect_true(all(vap_lgl(ls_object(), function(i) is.object(get(i)))))
  expect_true(all(vap_lgl(ls_function(), is.function)))
})
