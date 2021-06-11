#' Identical extensions
#'
#' Extensions for the use of `base::identical()`
#'
#' @param ... Vectors of values to compare, element-wise of equal length
#' @param params Additional params (as a named list of arguments for
#'   [base::identical])
#'
#' @return A `logical` vector of `TRUE`/`FALSE` of equal length of each `...`
#'   vector
#'
#' @examples
#' x <- y <- z <- 1:5
#' y[2] <- 3L
#' z[5] <- NA_integer_
#'
#' identical(x, y)        # compare entire vector
#' are_identical(x, y)    # element-wise
#' are_identical(x, y, z) # 3 or more vectors
#' @export

are_identical <- function(..., params = NULL) {
  x <- list(...)
  n <- length(x)

  if (length(unique(lengths(x))) != 1L) {
    stop("... must have equal length vectors", call. = FALSE)
  }

  if (n < 2L) {
    stop("... must have length of 2 or more", call. = FALSE)
  }

  if (n == 2L) {
    return(do_map_identical(x[[1]], x[[2]], params))
  }

  res <- list()

  for (i in seq_len(n - 1L)) {
    res[[i]] <- do_map_identical(x[[i]], x[[i + 1L]], params)
  }

  apply(Reduce(cbind, res), 1, all)
}

do_map_identical <- function(x, y, params = NULL) {
  mapply(
    function(.x, .y) {
      do.call(identical, c(list(x = .x, y = .y), params))
    },
    x,
    y,
    USE.NAMES = FALSE,
    SIMPLIFY = TRUE
  )
}
