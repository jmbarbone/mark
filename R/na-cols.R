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
