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
  stopifnot(is.vector(x, "list"))
  x[!vap_lgl(x, is.null)]
}

#' Selecting NA columns
#'
#' Select or remove columns that are entirely NA
#'
#' @param x A data.frame
#' @param names Logical, if `TRUE` (default) will return column names as names
#'   of vector
#'
#' @returns
#' * `select_na_cols()` the data.frame with only columns that are all `NA`
#' * `remove_na_cols()` the data.frame without columns of only `NA`
#' * `is_na_cols()` a logical vector: `TRUE` all rows of column are `NA`,
#'  otherwise `FALSE`
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

#' @rdname na_cols
#' @export
is_na_cols <- function(x, names = TRUE) {
  vap_lgl(x, function(xx) all(is.na(xx)), .nm = names)
}
