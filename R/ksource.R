#' Source an Rmd file
#'
#' Sources an Rmd file as a temp file
#'
#' @param x An Rmd file.
#' @param ... Additional arguments passes to \code{knitr::purl}
#' @param quiet Logical.  Determines whether to apply silence to \cide{knitr::purl}
#' @import knitr
#' @export

ksource <- function(x, ..., quiet = TRUE)
{
  source(kntir::purl(x, output = tempfile()), ..., quiet = quiet)
}
