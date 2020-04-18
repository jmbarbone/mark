#' Percentile rank
#'
#' Computes a percentile rank for each score in a set.
#' The bounds of a percentile rank are 0 - 99th percentile.
#' A percentile rank here is the proportion of scores that are less than
#'   the current score.
#'
#' @details
#' This is not a very fast formula, but it is correct.
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
#'
#' @examples
#' x <- c(1, 1, 2, 5, 7, 7, 8, 10)
#' percentile_rank(x)
#' \dontrun{
#' dplyr::percent_rank(x)
#' }

percentile_rank <- function(x) {
  p <- unname(proportion(x))
  (cumsum(p) - p + p * .5)[match(x, sort(unique(x)))]
}
