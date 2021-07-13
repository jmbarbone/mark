test_that("DESCRIPTION date is today", {
  skip_if_not(interactive(), "not interactive: check DESCRIPTION date")
  skip_if_not(package_available("desc"), "desc not available: check DESCRIPTION date")

  expect_identical(
    desc::desc_get_field("Date"),
    as.character(Sys.Date())
  )
})
