
is_named <- function(x) {
  !is.null(names(x))
}

#' Sort by names
#'
#' Sort a vector by it's name
#'
#' @param x A named vector of values
#' @param numeric If `TRUE` will try to coerce to numeric
#' @return `x` sorted by its `names()`
#' @export
sort_names <- function(x, numeric = FALSE) {
  check_is_vector(x)
  nm <- names(x) %||% stop("x must be a named vector", call. = FALSE)

  if (numeric) {
    nm <- as.numeric(nm)
  }

  sort_by(x, nm)
}

#' Set names
#'
#' Sets or removes names
#'
#' @param x A vector of values
#' @param nm A vector of names
#' @return
#' * `set_names0()`: `x` with `nm` values assigned to names (if `x` is `NULL`,
#'   `NULL` is returned)
#' * `remove_names()`: `x` without names
#' * `names_switch()`: `character` vector of equal length `x` where names and
#'    values are switched
#'
#' @export
#' @name set_names0
set_names0 <- function(x, nm = x) {
  .Deprecated("set_names")
  set_names(x = x, nm = nm)
}

#' @rdname set_names0
#' @export
names_switch <- function(x) {
  nm <- names(x) %||% stop("x must be named", call. = FALSE)
  set_names(nm, as.vector(x, "character"))
}
