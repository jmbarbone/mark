
test_that("tests with temp dir", {
  expect_equal_path <- function(x, y) {
    x_short <- fs::path(basename(dirname(x)), basename(x))
    y_short <- fs::path(basename(dirname(y)), basename(y))

    expect_true(file.exists(y), info = "Expected path does not exist")
    expect_equal(x_short, y_short)
  }

  td <- tempdir(check = TRUE)

  skip_if_not(rn("withr"))
  withr::with_tempdir({
    dates <- as.character(as.POSIXct("2021-01-02 15:57:25") + 1:3)
    dates <- gsub(":", "", dates)

    dirs <- c(
      dates,
      c(0, 100, 300, Inf)
    )

    files <- c(
      "no_ext"
    )

    invisible(
      sapply(dirs, function(x) {
        dir_create(file_path(td, x), overwrite = TRUE)
      })
    )

    # Add a second so times are unique
    Sys.sleep(1)
    most_recent_dir <- file_path(td, "most_recent_dir")
    dir_create(most_recent_dir, overwrite = TRUE)

    invisible(
      sapply(files, function(x) {
        file_create(file_path(td, x), overwrite = TRUE)
      })
    )

    # Add a second so times are unique
    Sys.sleep(1)
    most_recent_file <- file_path(td, "most_recent_file")
    # Should be able to create a file with the same name as a directory
    file_create(most_recent_file)

    expect_equal_path(get_recent_dir(td), most_recent_dir)
    expect_equal_path(get_dir_recent_date(td), file_path(td, dates[3]))
    expect_equal_path(get_dir_max_number(td), file_path(td, 300))
    expect_false(is_dir(file_path(td, "no_ext")))
    expect_equal_path(get_recent_file(td), most_recent_file)
  },
  tmpdir = td
  )
})

test_that("errors", {
  expect_error(is_file(NULL),        class = "simpleError")
  expect_error(is_file(character()), class = "simpleError")
  expect_error(is_file(TRUE),        class = "simpleError")
  expect_error(is_dir(NULL),         class = "simpleError")
  expect_error(is_dir(character()),  class = "simpleError")
  expect_error(is_dir(TRUE),         class = "simpleError")
})
