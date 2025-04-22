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
#' @return A single `integer`
#'
#' @export
#'
#' @examples
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
  if (no_length(x)) {
    # Empty list -- don't count
    return(0L)
  }

  if (length(x) == 1L && !is.list(x[[1]])) {
    # Check if next level is a list
    return(depth(x[[1]]))
  }

  # +1 for every level
  max(vap_int(x, depth) + 1L)
}
