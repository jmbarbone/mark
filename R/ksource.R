#' Sourcing extensions
#'
#' Functions for extending sourcing features
#'
#' @param file An R or Rmd file.
#' @param ... Additional arguments, see details.
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param cd Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#'
#' @details
#'
#' `try_source()` will output an error message rather than completely preventing the execution.
#' This can be useful for when a script calls on multiple, independent files to be sourced
#' and a single failure shouldn't prevent the entire run to fail as well.
#'
#' `ksource()` ... sends additional arguments to [knitr::purl()]
#'
#' @name sourcing
#' @export

ksource <- function(file, ..., quiet = TRUE, cd = FALSE)
{
  require_namespace("knitr")
  source(
    knitr::purl(
      file,
      output = tempfile(),
      quiet = quiet,
      ...
    ),
    chdir = cd
  )
}


#' @rdname sourcing
#' @export
try_source <- function(file, ..., cd = FALSE) {
  tryCatch(
    source(file, chdir = cd),
    error = function(e) {
      warning(e, call. = FALSE)
    },
    simpleWarning = function(e) {
      warning(e, call. = FALSE)
    })
}

#' @rdname sourcing
#' @export
try_ksource <- function(file, ..., cd = FALSE) {
  tryCatch(
    ksource(file = file, ..., cd = cd),
    error = function(e) {
      warning(e, call. = FALSE)
    },
    simpleWarning = function(e) {
      warning(e, call. = FALSE)
    })
}

