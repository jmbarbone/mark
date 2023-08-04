#' File globbing
#'
#' Helper function for globbing files
#'
#' @param x A vector of characters
#' @param glob,regexp Search expressions, as either wildcard globbing or as a
#'   regular expression.  `glob` is by default passed to regexp via
#'   [utils::glob2rx()]
#' @param ... Additional parameters passed to `grep`; `value` is ignored
#' @export
glob <- function(x, glob = NULL, regexp = NULL, ...) {
  if (!is.null(glob)) {
    if (!is.null(regexp)) {
      stop("either `glob` or `regexp` should be NULL", call. = FALSE)
    }
    regexp <- utils::glob2rx(glob)
  }

  if (!is.null(regexp)) {
    params <- list(...)
    params$pattern <- regexp
    params$x <- x
    params$value <- TRUE
    x <- do.call(grep, params)
  }

  x
}
