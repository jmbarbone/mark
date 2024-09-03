test_that("import() works", {
  import("tools", "file_ext")
  expect_identical(file_ext, tools::file_ext)
  expect_error(import("tools", "file_ext"), class = "importAssignedError")
  expect_error(import("tools", "file_ext", overwrite = TRUE), NA)
})
