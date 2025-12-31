#' Alpha base
#'
#' Base 26 conversion with letters
#'
#' @param x A string of letters.  Non characters are removed.
#' @param base A numeric
#' @return A vector of integers
#'
#' @export
#'
#' @examples
#' base_alpha("AB")
#' base_alpha("XFD")
#' base_alpha(c("JMB", "Jordan Mark", "XKCD"))
#' sum(base_alpha(c("x", "k", "c", "d")))

base_alpha <- function(x, base = 26) {
  x <- as.character(x)
  check_base_alpha(base, high = 26)
  vap_int(x, base_alpha_single, base = base)
}

alpha_base <- function(x, base = 26) {
  .Deprecated("base_alpha")
  base_alpha(x, base = base)
}

base_alpha_single <- function(x, base) {
  a <- chr_split(tolower(x)) %wi% letters
  a <- match(a, letters[1:base], nomatch = NA_integer_)

  if (anyNA(a)) {
    stop(base_conversion_error("alpha_limit", base = base, x = x))
  }

  n <- length(a)

  if (n == 0) {
    return(NA_integer_)
  }

  as.integer(sum(c(a[-n] * base^(1:(n - 1)), a[n])))
}

#' Base N conversion
#'
#' Convert between base numbers
#'
#' @param x A vector of integers
#' @param from,to An integer base to convert to and from; `from` must be an
#'   integer from `1` to `10` and `to` can currently only be `10`.
#'
#' @returns The A vector of integers converted from base `from` to base `to`
#' @export
#' @examples
#' base_n(c(24, 22, 16), from = 7)
base_n <- function(x, from = 10, to = 10) {
  if (!is.numeric(x)) {
    stop(class_error("must_be", x, "numeric"))
  }

  if (from == to) {
    return(x)
  }

  if (to != 10) {
    stop(base_conversion_error("ten"))
  }

  check_base(from)
  vap_int(x, base_n_single, base = from)
}

base_n_single <- function(x, base) {
  ints <- as.integer(chr_split(x))

  if (any(ints >= base, na.rm = TRUE)) {
    stop(base_conversion_error("single_limit", base = base, x = x))
  }

  seqs <- (length(ints) - 1L):0L
  as.integer(sum(mapply(function(i, s) i * base^s, i = ints, s = seqs)))
}

check_base_alpha <- function(b, high = 26) {
  if (is.character(b)) {
    b <- chr_split(b)

    if (length(b) != 1) {
      stop(base_conversion_error("alpha_length"))
    }

    b <- match(tolower(b), letters)
  }

  check_base(b, high = high)
}

check_base <- function(b, high = 9) {
  if (b %% 1 != 0) {
    stop(base_conversion_error("integer"))
  }

  if (b > high || b <= 1) {
    stop(base_conversion_error("limit", high = high))
  }
}


# conditions --------------------------------------------------------------

base_conversion_error := condition(
  function(type, base, x, high) {
    switch(
      type,
      alpha_limit = sprintf(
        'Cannot calculate alpha base "%s" for "%s" which has letters beyond "%s"', # nolint: line_length_linter.
        base,
        x,
        x[base]
      ),
      ten = "base_n() is currently only valid for conversions to base 10",
      single_limit = sprintf(
        paste0(
          "Cannot caluclate base \"%s\" for \"%s\" which has numbers greater",
          " than or equal to the base value"
        ),
        base,
        x
      ),
      alpha_length = "base must be of length 1",
      integer = "base must be an integer",
      limit = sprintf("base must be between 1 and %s", high),
      stop(internal_error(c("Unknown base conversion error type: ", type)))
    )
  },
  type = "error",
  exports = c("base_alpha", "base_n")
  # TODO include help
)
