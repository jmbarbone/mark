#' Percentile rank
#'
#' Computes a percentile rank for each score in a set.
#' The bounds of a percentile rank are > 0 and < 100.
#' A percentile rank here is the proportion of scores that are less than the
#'   current score.
#'
#' @details
#' This is not a very fast formula, however it is correct.
#'
#' \deqn{PR = (c_L + 0.5 f_i) / N * 100}
#'
#' Where
#'
#'   \eqn{c_L} is the frequency of scores less than the score of interest
#'
#'   \eqn{f_i} is the frequency of the score of interest
#'
#'
#' @param x A vector of values to rank
#' @export
#'
#' @return A percentile rank between 0L and 100L
#'
#' @examples
#' x <- c(1, 1, 2, 5, 7, 7, 8, 10)
#' percentile_rank(x)
#' \dontrun{
#' dplyr::percent_rank(x) * 100
#' }

percentile_rank <- function(x) {
  p <- unname(proportion(x))
  (cumsum(p) - p + p * .5)[match(x, sort(unique(x)))] * 100
}
