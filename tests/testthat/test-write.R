test_that("write_file_md5() works", {
  df <- data.frame(a = 1, b = 2)
  temp <- tempfile()
  on.exit(fs::file_delete(temp))

  expect_output(write_file_md5(df))
  expect_message(write_file_md5(df, temp), NA)
  expect_message(write_file_md5(df, temp), class = "markFileCopyMsMessage")

  # atomic
  expect_output(write_file_md5("lines"))
  expect_output(write_file_md5(list(a = 1)))
  expect_output(write_file_md5(matrix(1:4, 2)))
})

test_that("write_file_md5() types", {
  foo <- function(method, ...) {
    file <- tempfile()
    on.exit(file.remove(file))
    df <- data.frame(a = 1)
    expect_message(write_file_md5(df, file, method = method, ...), NA)
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

test_that("write_file_md5() errors", {
  df <- data.frame(a = 1)
  expect_error(
    write_file_md5(df, method = "foo"),
    class = "matchParamMatchError"
  )

  expect_error(
    write_file_md5(df, method = "dcf", append = TRUE)
  )
})
