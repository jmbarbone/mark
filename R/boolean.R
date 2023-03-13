#' To Boolean
#'
#' Convert a vector to boolean/logical
#'
#' @param x A vector of values
#' @param ... Additional arguments passed to methods
#' @param true A vector of values to convert to `TRUE`
#' @param false A vector of values to convert to `FALSE`
#' @return A `logical` vector of equal length as `x`
#' @export
to_boolean <- function(x, ...) {
  UseMethod("to_boolean", x)
}

#' @rdname to_boolean
#' @export
to_boolean.logical <- function(x, ...) {
  x
}

#' @rdname to_boolean
#' @export
to_boolean.numeric <- function(x, true = 1L, false = 0L, ...) {
  to_boolean_numeric(x, true = true, false = false)
}

#' @rdname to_boolean
#' @export
to_boolean.character <- function(x, true = NULL, false = NULL, ...) {
  if (is.null(true) && is.null(false)) {
    return(to_boolean_default(x))
  }

  stop("Not yet finished")
}

#' @rdname to_boolean
#' @export
to_boolean.factor <- function(x, true = NULL, false = NULL, ...) {
  if (is.null(true) && is.null(false)) {
    return(to_boolean_default(x))
  }

  stop("Not yet finished")
}

to_boolean_default <- function(x) {
  x <- fact(x)
  u <- levels(x)
  lu <- trimws(tolower(u))
  res <- rep_len(NA, length(u))
  res[lu %in% c("true", "t", "yes", "y")] <- TRUE
  res[lu %in% c("false", "f", "no", "n")] <- FALSE
  res[x]
}

to_boolean_numeric <- function(x, true, false) {
  out <- rep(NA, length(x))
  out[x %in% true] <- TRUE
  out[x %in% false] <- FALSE
  out
}
