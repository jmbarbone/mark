#' Percentile rank
#'
#' Computes a percentile rank for each score in a set.
#'
#' @description
#' The bounds of the percentile rank are > 0 and < 1
#'
#' A percentile rank here is the proportion of scores that are less than the
#'   current score.
#'
#' \deqn{PR = (c_L + 0.5 f_i) / N}
#'
#' Where
#'
#'   \eqn{c_L} is the frequency of scores less than the score of interest
#'
#'   \eqn{f_i} is the frequency of the score of interest
#'
#'
#' @param x A vector of values to rank
#' @param times A vector of the number of times to repeat `x`
#'
#' @return The percentile rank of `x` between 0 and 1, exclusive
#'
#' @examples
#' percentile_rank(0:9)
#' x <- c(1, 2, 1, 7, 5, NA_integer_, 7, 10)
#' percentile_rank(x)
#'
#' if (package_available("dplyr")) {
#'   dplyr::percent_rank(x)
#' }
#'
#' # with times
#' percentile_rank(7:1, c(1, 0, 2, 2, 3, 1, 1))
#' @export
percentile_rank <- function(x, times = NULL) {
  if (!is.null(times)) {
    stopifnot(length(x) == 1 | length(x) == length(times))
    return(percentile_rank_weighted(x, times))
  }

  id <- pseudo_id(x, na_last = FALSE)
  u <- attr(id, "uniques")
  p <- props(id, na.rm = TRUE)
  p <- remove_na(p)[order(remove_na(u))]
  (cumsum(p) - p * 0.5)[match(x, sort.int(u))]
}

percentile_rank_weighted <- function(u, times) {
  dupe_check(u)
  o <- rep(NA_integer_, length(u))
  ok <- !is.na(u)
  o1 <- order(u[ok])
  o[ok] <- o1
  u <- u[ok]
  # protect against rounding?
  pid <- pseudo_id(rep.int(u, times[ok]), na_last = FALSE)
  p <- props(pid)
  p <- set_names0(p[match(u, attr(pid, "uniques"))], u)
  p[is.na(p)] <- 0
  p <- p[o1]
  (cumsum(p) - p * 0.5)[o]
}
