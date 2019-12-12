#' Median absolute deviations
#'
#' Computes the median absolute deviation
#'
#' @param x A numeric vector of values.
#' @param constant The scaling factor.
#' @param ... additional arguments passes to `stats::mad`
#' @export

mads <- function(x, constant = 1.4826, ...) {
  (x - median(x)) / mad(x, constant = constant, ...)
}
