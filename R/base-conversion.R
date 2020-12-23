#' Alpha base
#'
#' Base 26 conversion with letters
#'
#' @param x A string of letters.  Non characters are removed.
#' @param base A numeric
#'
#' @export
#'
#' @examples
#' base_alpha("AB")
#' base_alpha("XFD")
#' base_alpha(c("JMB", "Jordan Mark", "XKCD"))
#' sum(base_alpha(c("x", "k", "c", "d")))

base_alpha <- function(x, base = 26)  {
  stopifnot(is.character(x))
  check_base_alpha(base, high = 26)
  vap_int(x, base_alpha_single, base = base)
}

alpha_base <- function(x, base = 26) {
  warning("Use jordan::base_alpha() instead", call. = FALSE)
  base_alpha(x, base = base)
}

base_alpha_single <- function(x, base) {
  a <- letters %wi% chr_split(tolower(x))
  a <- match(a, letters[1:base], nomatch = NA_integer_)

  if (anyNA(a)) {
    stop(sprintf('Cannot calculate alpha base "%s" for "%s" which has letters beyond "%s"',
                 base, x, x[base]),
         call. = FALSE)
  }

  n <- length(a)

  if (n == 0) {
    return(NA_integer_)
  }

  as.integer(sum(c(a[-n] * base^(1:(n - 1)), a[n])))
}


#' Base N conversion
#'
#' Convert between base numbers (1 through 10)
#'
#' @param x A vector of integers
#' @param base An integer base (valid 1 through 10)
#'
#' @export
base_n <- function(x, base = 10) {
  stopifnot(is.numeric(x))

  if (base == 10) {
    return(x)
  }

  check_base(base)
  vap_int(x, base_n_single, base = base)
}

base_n_single <- function(x, base) {
  ints <- as.integer(chr_split(x))

  if (any(ints >= base)) {
    stop("Cannot caluclate base \'", base, "\' for \'", x, "\' which has ",
         "numbers greater than or equal to the base value", call. = FALSE )
  }

  seqs <- (length(ints) - 1L):0L
  as.integer(sum(mapply(function(i, s) i * base^s, i = ints, s = seqs)))
}

check_base_alpha <- function(b, high = 26) {
  if (is.character(b)) {
    b <- chr_split(b)
    stopifnot(length(b) == 1)
    b <- match(tolower(b), letters)
  }

  check_base(b, high = high)
}

check_base <- function(b, high = 9) {
  stopifnot("base must be an integer" = b %% 1 == 0)

  if (b > high | b <= 1) {
    stop("base must be between 1 and ", high, call. = FALSE)
  }
}
