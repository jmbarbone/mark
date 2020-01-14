#' Source an Rmd file
#'
#' Sources an Rmd file as a temp file
#'
#' @param x An Rmd file.
#' @param ... Additional arguments passes to [knitr::purl()]
#' @param quiet Logical.  Determines whether to apply silence to `knitr::purl`
#' @export

ksource <- function(x, ..., quiet = TRUE)
{
  require_namespace("knitr")
  source(knitr::purl(x, output = tempfile(), quiet = quiet, ...))
}
