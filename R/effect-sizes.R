#' Tukey's coefficient
#'
#' Returns the coeficient for Tukey's boxplot
#'
#' @param x A vector of values
#'
#' @return The coefficient of the distance from Q1 or Q3 by IQR values
#'
#' @importFrom stats quantile
#' @importFrom dplyr case_when
#' @examples
#' tukey_coef(0:20)
#' @export

tukey_coef <- function(x) {
  stopifnot(is.numeric(x))
  q1 <- quantile(x, .25, names = F, na.rm = T)
  q3 <- quantile(x, .75, names = F, na.rm = T)
  iqr <- q3 - q1

  case_when(x < q1   ~ (x - q1) / iqr,
            x > q3   ~ (q3 - x) / iqr,
            is.na(x) ~ NaN,
            TRUE     ~ 0)
}

#' Z-score
#'
#' Compute z-score from a vector
#'
#' @param x A numeric vector of values.
#' @param na.rm Logical
#'
#' @importFrom stats sd
#' @export

z_score <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}


#' Odds ratio
#'
#' Returns the odds ratio without returning infinity
#'
#' @param a,b,c,d Values in the contingency table.
#' @param type Type to use
#' @return The odds ratio
#'
#' @export

odds_ratio <- function(a, b, c, d, type = "hits_misses") {
  ## does not return any Inf values

  if(a < 0 || b < 0 || c < 0 || d < 0) stop("Cells cannot have negative numbers.", call. = F)

  if(type == "hits_evals") {
    ## save over for single line at end
    c <- c - a
    d <- d - b
  }
  if((b == 0) || (c == 0)) return(NA)
  if(a < 5 || b < 5 || c < 5 || d < 5) warning("Cells should all have at least 5 observations.", call. = F)
  (a * d) / (b * c)
}


#' Odds ratio to Cohen's D
#'
#' @param odds An odds ratio
#' @param var Logical.
#'
#' @importFrom stats var
#' @export

odds2d <- function(odds, var = FALSE)
{
  if(var) {
    var(log(odds), na.rm = T) * sqrt(3) / pi^2
  }  else {
    log(odds) * sqrt(3) / pi
  }
}

#' Odds ratio to r
#'
#' Transforms an odds ratio to an r value
#'
#' @param odds A single odds ratio.
#' @param n1,n2 The ns of the groups.
#' @param var Logical.  Determines if variance should be computed.
#'
#' @export

odds2r <- function(odds, n1 = NULL, n2 = n1, var = FALSE)
{
  cohensd2r(odds2d(odds, var = var), n1 = n1, n2 = n2, var = var)
}

#' r to Cohen's D
#'
#' Converts an r value to Cohne's d
#'
#' @param r An r value
#'
#' @importFrom stats var
#' @export

r2cohensd <- function(r) {
  if(any(r > 1 || r < -1)) stop("r values outside bounds.", call. = F)
  if(var) {
    (4 * var(r, na.rm = T)) / ((1 - r^2)^3)
  } else {
    (2 * r) / sqrt(1 - r^2)
  }
}

#' r to Cohen's D
#'
#' Converts an r value to Cohen's D value
#'
#' @param d Cohen's d
#' @param n1,n2 Ns for the groups
#' @param var Logical.
#'
#' @importFrom stats var
#' @export

cohensd2r <- function(d, n1 = NULL, n2 = n1, var = F) {
  if(is.null(n1)) {
    a <- 4
  } else {
    a <- sum(n1, n2)^2 / prod(n1, n2)
  }
  if(var) {
    (a^2 * var(d, na.rm = T)) / ((d^2 + a)^3)
  } else {
    d / sqrt(d^2 + a)
  }
}
