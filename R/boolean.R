#' To Boolean
#'
#' Convert a vector to boolean/logical
#'
#' @details
#' [to_boolean.integer()] is passed to [to_boolean.numeric()] with
#' integer-specific `true` and `false` defaults.  [to_boolean.factor()] converts
#' the factor levels via [to_boolean.character()].
#'
#'
#' @param x A vector of values
#' @param ... Additional arguments passed to methods
#' @param true A vector of values to convert to `TRUE`
#' @param false A vector of values to convert to `FALSE`
#' @param na An optional vector of values to convert to `NA`; if set, an error
#'   will be thrown when any values in `x` are not matched to `true`, `false`,
#'   or `na`.
#' @return A `logical` vector of equal length as `x`.
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
to_boolean.numeric <- function(x, true = 1, false = 0, na = NULL, ...) {
  do_to_boolean(x, true, false, na, ...)
}

#' @rdname to_boolean
#' @export
to_boolean.integer <- function(x, true = 1L, false = 0L, ...) {
  to_boolean.numeric(x, true, false, ...)
}

#' @rdname to_boolean
#' @export
to_boolean.character <- function(
    x,
    true = c("TRUE", "true", "T", "t", "YES", "yes", "Y", "y"),
    false = c("FALSE", "false", "F", "f", "NO", "no", "N", "n"),
    na = NULL,
    ...
) {
  do_to_boolean(x, true, false, na, ...)
}

#' @rdname to_boolean
#' @export
to_boolean.factor <- function(x, ...) {
  lvls <- levels(x)
  if (anyNA(x) && !anyNA(lvls)) {
    lvls <- c(lvls, NA)
  }
  to_boolean(lvls, ...)[x]
}


do_to_boolean <- function(x, true, false, na) {
  if (anyDuplicated(c(true, false, na))) {
    stop(cond(
      "Cannot convert to boolean/logical: overlapping `true`, `false`, or ",
     "`na` values found",
      class = c("bad_bools_input", "error")
    ))
  }

  bool <- rep(NA, length(x))
  bool[x %in% true] <- TRUE
  bool[x %in% false] <- FALSE

  if (!is.null(na)) {
    m <- match(x, c(true, false, na))
    if (anyNA(m)) {
      error <- cond(
        "Cannot convert to boolean/logical: unmatched values found in `x`: ",
        collapse(unique(x[is.na(m)]), ", "),
        class = c("bad_bools_unmatched", "error")
      )
      stop(error)
    }
  }

  bool
}

cond <- function(..., class = NULL, call = NULL) {
  struct(
    list(paste0(..., collapse = ""), call),
    class = c(class, "condition"),
    names = c("message", "call")
  )
}
