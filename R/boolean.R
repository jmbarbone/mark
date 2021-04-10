#' To Boolean
#'
#' Convert a vector to boolean/logical
#'
#' @param x A vector of values
#' @param ... Additional arguments passed to methods
#' @param true A vector of values to convert to `TRUE`
#' @param false A vector of values to convert to `FALSE`
to_boolean <- function(x, ...) {
  UseMethod("to_boolean", x)
}

#' @rdname to_boolean
to_boolean.logical <- function(x, ...) {
  x
}

#' @rdname to_boolean
to_boolean.numeric <- function(x, true = 1L, false = 0L) {
  out <- rep(NA, length(x))
  out[x %in% true] <- TRUE
  out[x %in% false] <- FALSE
  out
}

#' @rdname to_boolean
to_boolean.character <- function(x, true = NULL, false = NULL) {
  if (is.null(true) && is.null(false)) {
    x <- fact(x)
    return(to_boolean_default(x))
  }
}

#' @rdname to_boolean
to_boolean.factor <- function(x, true, false) {
  if (is.null(true) && is.null(false)) {
    return(to_boolean_default(x))
  }
}

to_boolean_default <- function(x) {
  u <- levels(x)
  lu <- trimws(tolower(u))
  res <- rep_len(NA, length(u))
  res[lu %in% c("true", "t", "yes", "y")] <- TRUE
  res[lu %in% c("false", "f", "no", "n")] <- FALSE
  res[match(x, u)]
}
