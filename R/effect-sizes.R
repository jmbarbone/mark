#' Tukey's coefficient
#'
#' Returns the coefficient for Tukey's boxplot
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

#' Effect sizes conversions
#'
#' Calculate effect sizes and conversions
#'
#' @details
#' `odds` are log odds
#'
#' @param odds,r,cohend A vector of effect sizes
#' @param var Logical, if `TRUE` converts the variance instead
#' @param type Type of entry for [odds_ratio()]
#' @param a,b,c,d Values in the contingency table; valid if either `a`, `b`,
#'   `c` and `d` are the same length or if `a` has length of `4` and `b`, `c`,
#'   and `d` are `NULL`.
#' @param n1,n2 The ns of the groups
#'
#' @examples
#' x <- c(26, 13, 5, 6)
#' odds_ratio(x)           # 2.4
#' odds2r(2.4)             # 0.2346004
#' odds2d(2.4)             # 0.4826712
#' r2cohend(0.2346004)    # 0.4826712
#' cohend2odds(0.4826712) # 0.8754687
#' cohend2r(0.4826712)    # 0.2346004
#' exp(0.8754687)          # 2.4
#'
#'
#' @rdname effect_sizes
#' @export

odds_ratio <- function(a, b = NULL, c = NULL, d = NULL, type = "hits_misses")
{
  if (length(a) == 4 & is.null(b) & is.null(c) & is.null(d)) {
    d <- a[4]
    c <- a[3]
    b <- a[2]
    a <- a[1]
  }

  if(a < 0 | b < 0 | c < 0 | d < 0) {
    stop("Cells cannot have negative numbers", call. = FALSE)
  }

  if(type == "hits_evals") {
    c <- c - a
    d <- d - b
  }

  if((b == 0) | (c == 0))  {
    return(NaN)
  }

  if(a < 5 | b < 5 | c < 5 | d < 5) {
    warning("Cells should all have at least 5 observations",
            call. = FALSE)
  }

  (a * d) / (b * c)
}


#' @rdname effect_sizes
#' @export
odds2d <- function(odds, var = FALSE) {
  out <- log(odds) * sqrt(3) / pi

  if (var) {
    out <- out / pi
  }

  out
}


#' @rdname effect_sizes
#' @export
odds2r <- function(odds, n1 = 4, n2 = n1, var = FALSE) {
  mapply(
    function(odds, n1, n2, var) {
      cohend2r(odds2d(odds, var = var), n1 = n1, n2 = n2, var = var)
    },
    odds = odds,
    n1 = n1,
    n2 = n2,
    var = var,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

#' @rdname effect_sizes
#' @export
r2cohend <- function(r, var = FALSE) {
  nas <- is.na(r)
  res <- double(length(r))

  ok <- !nas

  if (!var) {
    ok <- r <= 1 & r >= -1

    if (any(!ok, na.rm = TRUE)) {
      warning("r values outside bounds", call. = FALSE)
    }

    ok[is.na(ok)] <- FALSE
    res[!ok] <- NaN
    r <- r[ok]
  }

  res[nas] <- NA_real_
  res[ok] <- if (var) {
    (4 * r) / ((1 - r^2)^3)
  } else {
    (2 * r) / sqrt(1 - r^2)
  }

  res
}

#' @rdname effect_sizes
#' @export
cohend2r <- function(cohend, n1 = NULL, n2 = n1, var = FALSE) {
  if (is.null(n1)) {
    a <- 4
  } else {
    a <- sum(n1, n2)^2 / prod(n1, n2)
  }
  if (var) {
    (a^2 * cohend) / ((cohend^2 + a)^3)
  } else {
    cohend / sqrt(cohend^2 + a)
  }
}

#' @rdname effect_sizes
#' @export
cohend2odds <- function(cohend, var = FALSE) {
  # log odds
  if (var) {
    cohend * pi^2 / 3
  } else {
    cohend * pi / sqrt(3)
  }
}

cohend2g <- function(d, df) {
  d * (1 - 3 / (4 * df - 1))
}
