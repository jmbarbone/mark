#' Fisher's method for combined probabilites
#'
#' Computes Fisher's method for combined probabilities
#'
#' @param .x A vector of p-values.
#' @param ... Additional arguments passed to `base:pchisq`
#' @importFrom stats pchisq
#' @export

fishers_method <- function(.x, ...) {
  valid <- .x[.x <= 1 & .x >= 0]
  ps <- valid[valid != 0]
  ps[ps == 0] <- min(valid)
  log_ps <- log(ps)
  estimate <- (-2) * sum(log_ps)
  pchisq(estimate, df = 2 * length(log_ps), lower.tail = F, ...)
}
