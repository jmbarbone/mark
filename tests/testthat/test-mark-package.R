test_that("DESCRIPTION date is today", {
  expect_identical(
    desc::desc_get_field("Date"),
    as.character(Sys.Date())
  )
})
