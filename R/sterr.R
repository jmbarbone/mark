#' Standard error
#'
#' Returns the standard error of a vector or columns of a data.frame
#'
#' @param x A vector or data.frame
#' @param na.rm Passed to `stats::sd()`.
#' @return A vector
#' @importFrom stats sd
#' @export

sterr <- function(x, na.rm = F)
{
  UseMethod("sterr", x)
}

sterr.numeric <- function(x, na.rm = F) {
  n <- length(x)
  sd <- sd(x, na.rm = na.rm)
  sd / sqrt(n)
}

sterr.data.frame <- function(x, na.rm = F) {
  vapply(x, sterr, double(1), na.rm = na.rm)
}
