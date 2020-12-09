test_that("Add worksheets", {
  wb <- openxlsx::createWorkbook()
  expect_error(add_data_sheet(wb, iris, "one", override = FALSE), NA)

  openxlsx::addWorksheet(wb, "two")
  expect_error(add_data_sheet(wb, iris, "two", override = FALSE))
  expect_error(add_data_sheet(wb, iris, "two", override = TRUE), NA)
})
