test_that("write_file_md5() works", {
  df <- quick_dfl(a = 1, b = 2)
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
    x <-
      if (method %in% c(mark_write_methods()$lines, "write")) {
        letters
      } else {
        quick_dfl(a = 1, b = "n", c = TRUE)
      }
      expect_message(
        write_file_md5(x, file, method = !!method),
        NA
      )
  }

  for (method in unlist0(mark_write_methods())) {
    foo(method)
  }
})

test_that("path warning", {
  t <- tempfile()
  on.exit(fs::file_delete(t))
  x <- structure(quick_dfl(a = 1), path = t)
  expect_warning(
    write_file_md5(x, t),
    "attr(x, \"path\") is being overwritten",
    fixed = TRUE
  )
})

test_that("write_file_md5() errors", {
  df <- quick_dfl(a = 1)
  expect_error(
    write_file_md5(df, method = "foo"),
    class = "matchParamMatchError"
  )
})

test_that("compression works", {
  foo <- function(ext = "") {
    file <- tempfile(fileext = ext)
    on.exit(unlink(file, recursive = TRUE))
    df <- quick_dfl(a = 1)
    write_file_md5(df, file)
  }

  expect_s3_class(foo(), "data.frame")
  expect_s3_class(foo(".csv.gz"), "data.frame")
  expect_s3_class(foo(".tsv.bz2"), "data.frame")
  expect_s3_class(foo(".dcf.xz"), "data.frame")
  expect_s3_class(foo(".csv.zip"), "data.frame")
  expect_s3_class(foo(".tar.gz"), "data.frame")

  expect_error(
    compress("foo.tar.zip"),
    "'zip' is not a valid method",
    fixed = TRUE
  )
})

test_that("list columns", {
  foo <- function(method) {
    temp <- tempfile(fileext = ".csv")
    on.exit(safe_fs_delete(temp))

    op <- options(mark.list.hook = method)
    on.exit(options(op))

    df <- quick_dfl(x = c("a", "b"))
    df$y <- list(1:2, 2)

    write_file_md5(df, temp)
    fs::file_delete(temp)

    df$z <- list(1, 2:3)
    write_file_md5(df, temp)
  }

  expect_s3_class(foo("auto"), "data.frame")
  expect_s3_class(foo("default"), "data.frame")
  expect_s3_class(foo("json"), "data.frame")
  expect_s3_class(foo(toString), "data.frame")
  expect_s3_class(foo(TRUE), "data.frame")
  expect_s3_class(foo(FALSE), "data.frame")
  expect_s3_class(foo("toString"), "data.frame")
  expect_error(
    foo("none"),
    "unimplemented type 'list' in 'EncodeElement'",
    fixed = TRUE
  )
  expect_error(foo(NA), class = "writeFileMd5ListHookError")
})

test_that("arrow prints something to stdout()", {
  expect_snapshot(write_file_md5(quick_dfl(a = 1), method = "feather"))
  expect_snapshot(write_file_md5(quick_dfl(a = 1), method = "parquet"))
})
