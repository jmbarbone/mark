#' Pooled standard deviation
#'
#' Computes the pooled standard error
#'
#' @param ns A vector of N values.
#' @param ses A vector of standard errors.
#' @param max Logical value.  Whether to compute the max pooled deviation.
#' @import stats
#' @export

sd_pooled <- function(ns, ses, max = F) {
  st_devs <- ses * sapply(ns, sqrt)
  if(max) {a <- 0} else {a <- -length(st_devs)}
  sqrt(sum(sapply(ns, function(x) x - 1) * sapply(st_devs, function(x) x^2)) / sum(ns, a))
}
