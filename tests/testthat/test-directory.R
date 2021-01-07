# unlink(tempdir(), recursive = TRUE)

test_that("tests with temp dir", {
  td <- tempdir(check = TRUE)

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

    expect_equal(get_recent_dir(td), most_recent_dir)
    expect_equal(get_dir_recent_date(td), file_path(td, dates[3]))
    expect_equal(get_dir_max_number(td), file_path(td, 300))
    expect_false(is_dir(file_path(td, "no_ext")))
    expect_equal(get_recent_file(td), most_recent_file)
  },
  tmpdir = td
  )
})
