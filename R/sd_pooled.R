#' Pooled standard deviation
#'
#' Computes the pooled standard error
#'
#' @param ns A vector of N values.
#' @param ses A vector of standard errors.
#' @param max Logical value.  Whether to compute the max pooled deviation.
#' @export

sd_pooled <- function(ns, ses, max = FALSE) {
  st_devs <- ses * sapply(ns, sqrt)
  a <- ifelse(max, 0, -length(st_devs))
  sqrt(sum(sapply(ns, function(x) x - 1) * sapply(st_devs, function(x) x^2)) / sum(ns, a))
}
