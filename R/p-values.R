#' Round p-value
#'
#' Rounds a p-value by decimal places and reports with sig figs
#'
#' @param x A vector of p-values
#' @param n Number of digits to round by (if NULL - no rounding occurs)
#' @param sig Number of significant figures (if NULL - not used)
#' @param one,two,three,four The significance thresholds (defaults to .001, .01, .05, and .10).
#'
#' @export
#' @name p_values
#'
#' @examples
#' x <- stats::pchisq(abs(runif(25)), 3)
#' print(data.frame(x = x,
#'                  p = p_round(x),
#'                  sigs = p_value_sig(x)),
#'       digits = 3,
#'       right = FALSE)

p_round <- function(x, n = 3, sig = n) {
  valid_n <- !is.null(n)
  if (valid_n) {
    stopifnot(n > 1L)
    x <- round(x, n)
  }

  out <- character(length(x))
  nans <- is.nan(x)
  out[nans] <- "(NaN)"

  if (valid_n) {
    out[!nans & x == 0L] <- sprintf("< .%s1", paste(rep("0", n - 1), collapse = ""))
  }
  non_zeros <- !nans & x > 0

  if (length(non_zeros) > 0L & !is.null(sig)) {
    out[non_zeros] <- as.character(signif(x[non_zeros], sig))
  }

  out
}

#' @rdname p_values
#' @export

p_value_sig <- function(x, one = .001, two = .01, three = .05, four = .10) {
  stopifnot(inherits(x, "numeric"))
  out <- character(length(x))
  out[x < one] <- "***"
  out[x < two & x >= one] <- "**"
  out[x < three & x >= two] <- "*"
  out[x < four & x >= three] <- "."
  out
}
