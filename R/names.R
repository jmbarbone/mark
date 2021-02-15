
is_named <- function(x) {
  !is.null(names(x))
}

#' Sort by names
#'
#' Sort a vector by it's name
#'
#' @param x A vector
#'
#' @export
sort_names <- function(x) {
  if (!is_named(x) || !is.vector(x)) {
    stop("x must be a named vector", call. = FALSE)
  }

  x[sort(names(x))]
}

#' Set names
#'
#' Sets or removes names
#'
#' @param x A vector of values
#' @param nm A vector of names
#'
#' @export
set_names0 <- function(x, nm = x) {
  `names<-`(x, nm)
}

#' @rdname set_names0
#' @export
remove_names <- function(x) {
  set_names0(x, NULL)
}
