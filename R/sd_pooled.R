#' Pooled standard deviation
#'
#' Computes the pooled standard error
#'
#' @param ns A vector of N values.
#' @param ses A vector of standard errors.
#' @param max Logical value.  Whether to compute the max pooled deviation.
#' @export

sd_pooled <- function(ns, ses, max = FALSE) {
  st_devs <- ses * vap_dbl(ns, sqrt)

  a <- if (max) {
    0
  } else {
    -length(st_devs)
  }

  sqrt(sum(vap_dbl(ns, function(x) x - 1) * vap_dbl(st_devs, function(x) x^2)) / sum(ns, a))
}
