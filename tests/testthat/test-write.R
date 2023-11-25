test_that("write_file_ms() works", {
  df <- data.frame(a = 1, b = 2)
  temp <- tempfile()
  on.exit(fs::file_delete(temp))

  expect_output(write_file_ms(df))
  expect_message(write_file_ms(df, temp), NA)
  expect_message(write_file_ms(df, temp), class = "markFileCopyMsMessage")

  # atomic
  expect_output(write_file_ms("lines"))
  expect_output(write_file_ms(list(a = 1)))
  expect_output(write_file_ms(matrix(1:4, 2)))
})

test_that("write_file_ms() types", {
  foo <- function(method, ...) {
    file <- tempfile()
    on.exit(file.remove(file))
    df <- data.frame(a = 1)
    expect_message(write_file_ms(df, file, method = method, ...), NA)
  }

  foo("csv")
  foo("csv2")
  foo("dcf")
  foo("json")
  foo("rds")
  foo("table")
  foo("tsv")
  foo("tsv2")
  foo("json")
  foo("yaml")
})

test_that("write_file_ms() errors", {
  df <- data.frame(a = 1)
  expect_error(
    write_file_ms(df, method = "foo"),
    class = "matchParamMatchError"
  )

  expect_error(
    write_file_ms(df, method = "dcf", append = TRUE)
  )
})

# covr::file_report(covr::file_coverage("R/write.R", "tests/testthat/test-write.R"))
