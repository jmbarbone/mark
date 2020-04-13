#' Fisher's method for combined probabilities
#'
#' Computes Fisher's method for combined probabilities
#'
#' @param x A vector of p-values.
#' @importFrom stats pchisq
#' @export

fishers_method <- function(x) {
  valid <- x[x <= 1 & x >= 0]
  ps <- valid[valid != 0]
  ps[ps == 0] <- min(valid)
  pchisq((-2) * sum(log(ps)),
         df = 2 * length(ps),
         lower.tail = FALSE)
}
