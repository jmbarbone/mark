#' Percentile rank
#'
#' Computes a percentile rank for each score in a set.
#'
#' @description
#' The bounds of the percentile rank are > 0 and < 1 (see Boundaries)
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
#' @section Boundaries:
#'
#' While the percentile rank of a score in a set must be exclusively within the
#' boundaries of `0` and `1`, this function may produce a percentile rank that
#' is exactly `0` or `1`.  This may occur when the number of values are so large
#' that the value within the boundaries is too small to be differentiated.
#'
#' Additionally, when using the `weights` parameter, if the lowest or highest
#' number has a value of `0`, the number will then have a theoretical `0` or
#' `1`, as these values are not actually within the set.
#'
#' @param x A vector of values to rank
#' @param weights,times A vector of the number of times to repeat `x`
#'
#' @return The percentile rank of `x` between 0 and 1 (see Boundaries)
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
percentile_rank <- function(x, weights = times, times) {
  if (!missing(times)) {
    warning("`times` is deprecated; use `weights` instead", call. = FALSE)
  }

  times <- NULL
  force(weights)

  if (!is.null(weights)) {
    return(do_percentile_rank(x, weights))
  }

  id <- pseudo_id(x)
  tab <- counts(id)
  key <- attr(id, "uniques")
  res <- set_names0(do_percentile_rank(key, tab), NULL)
  set_names0(res[match(x, key)], x)
}

do_percentile_rank <- function(u, w) {
  dupe_check(u)
  w <- as.integer(w)
  if (length(w) == 1L) {
    if (is.na(w)) {
      # If weight is NA return NA?  Maybe through an warning?
      return(rep.int(NA_real_, length(n)))
    }

    # no ordering necessary
    ok <- !is.na(u)
    n <- sum(ok)
    p <- rep(1L, n)
    res <- (cumsum(p) - 0.5) / n
  } else {
    if (length(w) != length(u)) {
      stop("length(weights) must be 1L or equal to length(x)", call. = FALSE)
    }

    ok <- stats::complete.cases(u, w)
    o <- order(u[ok])
    p <- w[ok][o]
    res <- (cumsum(p) - p * 0.5)[match(u[ok], u[ok][o])] / sum(w[ok])
  }

  out <- rep(NA_real_, length(ok))
  names(out) <- u
  out[ok] <- res
  out
}
