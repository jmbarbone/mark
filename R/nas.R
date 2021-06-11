#' Remove NA
#'
#' Remove NAs from a vector
#'
#' @param x A vector
#' @return `x` without values where `is.na(x)` is `TRUE`
#' @export
remove_na <- function(x) {
  if (!is.vector(x)) {
    stop("x must be a vector", call. = FALSE)
  }

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
#' @return The list `x` without `NULL`
#' @examples
#' x <- list(a = letters[1:5], b = NULL, c = complex(3))
#' x
#' remove_null(x)
#' @export
remove_null <- function(x) {
  if (!is.vector(x, "list")) {
    stop("x must be a list", call. = FALSE)
  }

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
  if (!is.data.frame(x)) {
    stop("x must be a data.frame", call. = FALSE)
  }

  x[, is_na_cols(x)]
}

#' @rdname na_cols
#' @export
remove_na_cols <- function(x) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame", call. = FALSE)
  }

  x[, !is_na_cols(x)]
}

#' @rdname na_cols
#' @export
is_na_cols <- function(x, names = TRUE) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame", call. = FALSE)
  }

  vap_lgl(x, function(xx) all(is.na(xx)), .nm = names)
}
