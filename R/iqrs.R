#' Inter Quartile Ranges
#'
#' Computes the IQR magnitude of a vector
#'
#' @param x A vector of values
#' @param na.rm Logical. If `TRUE`, any `NA` and `NaN`'s are removed from `x` before the median and quantiles are computed.
#'
#' @importFrom stats median
#' @importFrom stats quantile
#'
#' @export
#' @examples
#' iqrs(stats::rchisq(100, 2))

iqrs <- function(x, na.rm = FALSE) {
  (x - median(x, na.rm = na.rm)) / diff(quantile(x, c(.25, .75), names = FALSE, na.rm = na.rm))
}
