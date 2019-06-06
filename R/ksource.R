ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
