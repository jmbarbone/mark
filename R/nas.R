#' Remove NA
#'
#' Remove NAs from a vector
#'
#' @param x A vector
#'
#' @export
remove_na <- function(x) {
  stopifnot(is.vector(x))
  x[!is.na(x)]
}

unique_no_na <- function(x) {
  unique(remove_na(x))
}

#' Remove NULL
#'
#' Remove NULL results from a list
#'
#' @param x A list
#' @examples
#' x <- list(a = letters[1:5], b = NULL, c = complex(3))
#' x
#' remove_null(x)
#' @export
remove_null <- function(x) {
  stopifnot(is.list(x))
  x[!vap_lgl(x, is.null)]
}

#' Selecting NA columns
#'
#' Select or remove columns that are entirely NA
#'
#' @param x A data.frame
#' @name na_cols
#' @export

select_na_cols <- function(x) {
  stopifnot(is.data.frame(x))
  x[, is_na_cols(x)]
}

#' @rdname na_cols
#' @export
remove_na_cols <- function(x) {
  stopifnot(is.data.frame(x))
  x[, !is_na_cols(x)]
}


is_na_cols <- function(x) {
  vap_lgl(x, function(xx) all(is.na(xx)))
}
