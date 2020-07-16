#' Depth
#'
#' Functions to extract the 'depth' of an object
#'
#' @details
#' This function does not count an empty lists (`list()`) as a level or `NULL`
#'   objects.
#'
#' @param x An object
#' @param ... Possible additional arguments passed to methods (not in use)
#'
#' @export
#'
#' @examples
#'
#' a <- c(1, 2, 3)
#' depth(a) # Vectors are 1L
#'
#' b <- list(a = 1, b = list(list(1)))
#' depth(b)

depth <- function(x, ...) {
  UseMethod("depth", x)
}

#' @export
#' @rdname depth
depth.default <- function(x, ...) {
  if (is.null(x)) {
    0L
  } else {
    1L
  }
}

#' @export
#' @rdname depth
depth.list <- function(x, ...) {
  if (length(x) == 0L) {
    # Empty list -- don't count
    return(0L)
  } else if (length(x) == 1L & !is.list(x[[1]])) {
    # Check if next level is a list
    depth(x[[1]])
  } else {
    # +1 for every level
    max(vapply(x, depth, integer(1)) + 1L)
  }
}
