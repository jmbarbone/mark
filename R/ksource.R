#' Sourcing extensions
#'
#' Functions for extending sourcing features
#'
#' @param file An R or Rmd file.
#' @param ... Additional arguments, see details.
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param change_directory Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#'
#' @details
#'
#' `try_source()` will output an error message rather than completely preventing the execution.
#' This can be useful for when a script calls on multiple, independent files to be sourced
#' and a single failure shouldn't prevent the entire run to fail as well.
#'
#' `ksource()` ... sends additional arguemnts to [knitr:purl()]
#'
#' @name sourcing
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


#' @rdname sourcing
#' @export
try_source <- function(file, ..., change_directory = FALSE) {
  tryCatch(source(file,
                  chdir = change_directory),
           simpleWarning = function(e) warning(e, call. = TRUE))
}

#' @rdname sourcing
#' @export
try_ksource <- function(file, ..., change_directory = FALSE) {
  tryCatch(source(knitr::purl(file,
                              output = tempfile(),
                              quiet = quiet,
                              ...),
                  chdir = change_directory),
           simpleWarning = function(e) warning(e, call. = TRUE))
}

