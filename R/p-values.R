#' Round p-value
#'
#' Rounds a p-value by decimal places and reports with sig figs
#'
#' @param x A vector of p-values
#' @param n Number of digits to round by (if NULL - no rounding occurs)
#' @param sig Number of significant figures (if NULL - not used)
#' @param cutoffs A named vector for significant cutoffs
#'
#' @export
#' @name p_values
#'
#' @examples
#' set.seed(42)
#' x <- stats::pchisq(abs(runif(25)), 4)
#' print(data.frame(x = x,
#'                  p = p_round(x),
#'                  sigs = p_value_sig(x)),
#'       digits = 3,
#'       right = FALSE)

p_round <- function(x, n = 3, sig = n) {
  valid_n <- !is.null(n)

  if (valid_n) {
    below <- x < (1 / (10^(n)))
    stopifnot(n > 1L)
    x <- round(x, n)
  }

  out <- character(length(x))
  nans <- is.nan(x)
  out[nans] <- "(NaN)"

  if (valid_n) {
    out[!nans & below] <- sprintf("< .%s1", paste(rep("0", n - 1), collapse = ""))
  }

  empty <- out == ""
  # formatc(., digits = NULL) is default
  # Cannot do this as one vector -- sigs should be evaluated item by item
  out[empty] <- vapply(x[empty], format, character(1), digits = sig, scientific = FALSE, USE.NAMES = FALSE)

  out
}

#' @rdname p_values
#' @export

p_value_sig <- function(x, cutoffs = c("***" = 0.001,
                                       "**" = 0.01,
                                       "*" = 0.05,
                                       "." = 0.10)) {
  cutoffs <- sort(cutoffs, decreasing = FALSE)
  cutoffs <- append(cutoffs, c(" " = 1))
  nm <- names(cutoffs)
  stopifnot("cuttoffs must be numeric" = is.numeric(cutoffs),
            "cuttoffs must be named" = !is.null(nm),
            "x must be 1 or less" = all(x <= 1, na.rm = TRUE))
  vapply(x,
         function(p) {
           nm[min(which(cutoffs >= p),
                  na.rm = TRUE)]
         },
         character(1),
         USE.NAMES = FALSE)
}
