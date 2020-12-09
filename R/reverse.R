#' Reverse an object
#'
#' Reverse an object
#'
#' @param x An object
#' @param ... Additional arguments passed to methods
#' @param keep_rownames Logical, if `TRUE` will not reset rownames; `NULL`
#'   will function as `TRUE` if rownames are simply 1:nrow(x)
#' @export

reverse <- function(x, ...) {
  UseMethod("reverse")
}

#' @rdname reverse
#' @export
reverse.default <- function(x, ...) {
  n <- length(x)

  if (!n) {
    return(x)
  }

  x[n:1]
}

#' @rdname reverse
#' @export
reverse.matrix <- function(x, keep_rownames = NULL, ...) {
  n <- nrow(x)

  if (!n) {
    return(x)
  }

  out <- x[n:1, ]
  rn <- attr(x, "row.names")

  if (is.null(keep_rownames)) {
    keep_rownames <- !identical(rn, 1:n)
  }

  if (!keep_rownames) {
    attr(out, "row.names") <- rn
  }

  out
}

#' @rdname reverse
#' @export
reverse.data.frame <- function(x, ...) {
  reverse.matrix(x, ...)
}
