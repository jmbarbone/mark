#' Source an Rmd file
#'
#' Sources an Rmd file as a temp file
#'
#' @param file An Rmd file.
#' @param ... Additional arguments passes to [knitr::purl()]
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param change_directory Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#' @export

ksource <- function(file, ..., quiet = TRUE, change_directory = FALSE)
{
  require_namespace("knitr")
  source(knitr::purl(file, output = tempfile(), quiet = quiet, ...), chdir = change_directory)
}
