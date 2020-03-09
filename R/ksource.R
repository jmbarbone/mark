#' Source an Rmd file
#'
#' Sources an Rmd file as a temp file
#'
#' @param file An Rmd file.
#' @param ... Additional arguments passes to [knitr::purl()]
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param change_directory Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#'
#' @export

ksource <- function(file, ..., quiet = TRUE, change_directory = FALSE)
{
  require_namespace("knitr")
  source(knitr::purl(file,
                     output = tempfile(),
                     quiet = quiet,
                     ...),
         chdir = change_directory)
}


#' Try to source a file
#'
#' This will output an error message rather than completely preventing the execution.
#' This can be useful for when a script calls on multiple, independent files to be sourced
#' and a single failure shouldn't prevent the entire run to fail as well.
#'
#' @param file An R or Rmd file
#' @inheritParams ksource
#'
#' @export

try_source <- function(file, ..., change_directory = FALSE) {
  tryCatch(source(file,
                  chdir = change_directory),
           simpleWarning = function(e) warning(e, call. = TRUE))
}

#' @rdname try_source
#' @export
try_ksource <- function(file, ..., change_directory = FALSE) {
  tryCatch(source(knitr::purl(file,
                              output = tempfile(),
                              quiet = quiet,
                              ...),
                  chdir = change_directory),
           simpleWarning = function(e) warning(e, call. = TRUE))
}

# try_source("lab/test-sourcing.R")
