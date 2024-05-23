test_that("write_file_md5() works", {
  df <- data.frame(a = 1, b = 2)
  temp <- withr::local_tempfile()
  expect_output(write_file_md5(df))
  expect_message(write_file_md5(df, temp), NA)
  expect_message(write_file_md5(df, temp), class = "markFileCopyMsMessage")

  # atomic
  expect_output(write_file_md5("lines"))
  expect_output(write_file_md5(list(a = 1)))
  expect_output(write_file_md5(matrix(1:4, 2)))
})

test_that("write_file_md5() types", {
  foo <- function(method) {
    file <- tempfile()
    on.exit(file.remove(file))
    df <- data.frame(a = 1, b = "n", c = TRUE)
    expect_message(write_file_md5(df, file, method = method), NA)
  }

  foo("csv")
  foo("csv2")
  foo("csv3")
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
})

test_that("compression works", {
  foo <- function(ext = "") {
    file <- tempfile(fileext = ext)
    on.exit(unlink(file, recursive = TRUE))
    df <- data.frame(a = 1)
    write_file_md5(df, file)
  }

  expect_s3_class(foo(), "data.frame")
  expect_s3_class(foo(".csv.gz"), "data.frame")
  expect_s3_class(foo(".tsv.bz2"), "data.frame")
  expect_s3_class(foo(".dcf.xz"), "data.frame")
  expect_s3_class(foo(".csv.zip"), "data.frame")
  expect_s3_class(foo(".tar.gz"), "data.frame")
})

test_that("list columns", {
  foo <- function(method) {
    temp <- tempfile(fileext = ".csv")
    on.exit(safe_fs_delete(temp))

    op <- options(mark.list.hook = method)
    on.exit(options(op))

    df <- data.frame(x = c("a", "b"))
    df$y <- list(1:2, 2)

    write_file_md5(df, temp)
  }

  expect_s3_class(foo("auto"), "data.frame")
  expect_s3_class(foo(toString), "data.frame")
  expect_s3_class(foo(TRUE), "data.frame")
  expect_s3_class(foo(NULL), "data.frame")
  expect_s3_class(foo(FALSE), "data.frame")
  expect_error(foo(NA))
})
