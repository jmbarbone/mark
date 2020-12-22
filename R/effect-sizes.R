#' Tukey's coefficient
#'
#' Returns the coeficient for Tukey's boxplot
#'
#' @param x A vector of values
#'
#' @return The coefficient of the distance from Q1 or Q3 by IQR values
#'
#' @examples
#' tukey_coef(0:20)
#' @export

tukey_coef <- function(x) {
  stopifnot(is.numeric(x))
  q1 <- stats::quantile(x, .25, names = FALSE, na.rm = TRUE)
  q3 <- stats::quantile(x, .75, names = FALSE, na.rm = TRUE)
  iqr <- q3 - q1

  dplyr::case_when(
    x < q1   ~ (x - q1) / iqr,
    x > q3   ~ (q3 - x) / iqr,
    is.na(x) ~ NA_real_,
    TRUE     ~ 0
  )
}

#' Z-score
#'
#' Compute z-score from a vector
#'
#' @param x A numeric vector of values.
#' @param na.rm Logical
#'
#' @export

z_score <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)
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

  if(a < 0 | b < 0 | c < 0 | d < 0) {
    stop("Cells cannot have negative numbers", call. = FALSE)
  }

  if(type == "hits_evals") {
    ## save over for single line at end
    c <- c - a
    d <- d - b
  }
  if((b == 0) | (c == 0)) return(NA)
  if(a < 5 | b < 5 | c < 5 | d < 5) {
    warning("Cells should all have at least 5 observations",
            call. = FALSE)
  }
  (a * d) / (b * c)
}


#' Odds ratio to Cohen's D
#'
#' @param odds An odds ratio
#' @param var Logical, if `TRUE` calculates the variances of d
#' @param na.rm Logical, if TRUE `NA` values are removed
#'   (only needed for `var = TRUE`)
#'
#' @examples
#' odds2d(c(1.1, .001, 2, NA)) # cohen's D for each
#' odds2d(c(1.1, .001, 2, NA), var = TRUE)
#' odds2d(c(1.1, .001, 2, NA), var = TRUE, na.rm = TRUE)
#'
#' @export

odds2d <- function(odds, var = FALSE, na.rm = FALSE) {
  if (var) {
    if (na.rm) {
      odds <- remove_na(odds)
    }

    if (length(odds) < 2) {
      warning("`odds` needs a vector of length at least 2", call. = FALSE)
      return(NA_real_)
    }

    stats::var(log(odds)) * sqrt(3) / pi^2
  } else {
    log(odds) * sqrt(3) / pi
  }
}



#' Odds ratio to r
#'
#' Transforms an odds ratio to an r value
#'
#' @param odds A single odds ratio.
#' @param n1,n2 The ns of the groups.
#'
#' @export

odds2r <- function(odds, n1, n2 = n1) {
  cohensd2r(odds2d(odds), n1 = n1, n2 = n2, var = var)
}

#' r to Cohen's D
#'
#' Converts an r value to Cohne's d
#'
#' @param r An r value
#'
#' @export

r2cohensd <- function(r) {
  if(any(r > 1 || r < -1)) {
    stop("r values outside bounds", call. = FALSE)
  }
  if(var) {
    (4 * stats::var(r, na.rm = TRUE)) / ((1 - r^2)^3)
  } else {
    (2 * r) / sqrt(1 - r^2)
  }
}

#' Cohen's D to r
#'
#' Converts Cohen's D to an r value
#'
#' @param d Cohen's d
#' @param n1,n2 Ns for the groups
#' @param var Logical.
#'
#' @export

cohensd2r <- function(d, n1, n2 = n1, var = FALSE) {
  if(is.null(n1)) {
    a <- 4
  } else {
    a <- sum(n1, n2)^2 / prod(n1, n2)
  }
  if(var) {
    (a^2 * stats::var(d, na.rm = TRUE)) / ((d^2 + a)^3)
  } else {
    d / sqrt(d^2 + a)
  }
}
