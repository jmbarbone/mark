test_that("find_author() works", {
  withr::local_options(list(mark.author = NULL))
  expect_error(find_author())
})

test_that("use_author() works", {
  withr::local_options(list(
    mark.check_interactive = NA,
    mark.author = list(
      given = "Jordan Mark",
      family = "Barbone",
      role = c("aut","cph", "cre"),
      email = "jmbarbone@gmail.com",
      comment = c(ORCID = "0000-0001-9788-3628")
    )
  ))
  withr::local_dir(test_path("files"))
  # get original version
  original <- readLines("DESCRIPTION")
  use_author()
  # restore

  expect_identical(get_version(), as.package_version("0.0.0"))
  bump_version()
  expect_identical(get_version(), as.package_version("0.0.1"))

  update_version("1.0.0")
  expect_identical(get_version(), as.package_version("1.0.0"))

  update_version("0.2021.10.10")
  expect_identical(get_version(), as.package_version("0.2021.10.10"))

  bump_date_version()
  expect_identical(get_version(), as.package_version("0.2021.10.10.1"))

  writeLines(original, "DESCRIPTION")

  expect_error(use_author(NULL), "list")
  expect_error(use_author(person()), "person")
})
