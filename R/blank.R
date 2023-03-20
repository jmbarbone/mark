#' Blank values
#'
#' Detect _blank_ values; select, remove columns that are entirely _blank_
#'
#' @details _Blank_ values are values that do not contain any text
#'
#' @param x An object, or `data.frame` for `*_cols()` functions
#' @param names Logical, if `TRUE` (default) will return column names as names
#'   of vector
#' @param na_blank Logical, if `TRUE` treats `NA` values as _blank_
#' @param ws Logical, when `TRUE` treats elements that are entirely _whitespace_
#'   as blanks
#'
#' @returns
#' * `is_blank()` a `logical` vector indicating _blank_ elements in `x`
#' * `select_blank_cols()` `x` with only columns that are all _blank_
#' * `remove_blank_cols()` `x` without columns of only _blank_
#' * `is_blank_cols()` a logical vector: `TRUE` all rows of column are _blank_,
#' otherwise `FALSE`
#' @name blank_values
NULL

#' @rdname blank_values
#' @export
is_blank <- function(x, na_blank = FALSE, ws = TRUE) {
  !is_true(nzchar(x, keepNA = na_blank)) |
    if (ws) grepl(x, pattern = "^[[:space:]]+$") else FALSE
}

#' @rdname blank_values
#' @export
select_blank_cols <- function(x, na_blank = FALSE, ws = TRUE) {
  x[, is_blank_cols(x, na_blank = na_blank, ws = ws), drop = FALSE]
}

#' @rdname blank_values
#' @export
remove_blank_cols <- function(x, na_blank = FALSE, ws = TRUE) {
  x[, !is_blank_cols(x, na_blank = na_blank, ws = ws), drop = FALSE]
}

#' @rdname blank_values
#' @export
is_blank_cols <- function(x, names = TRUE, na_blank = FALSE, ws = TRUE) {
  stopifnot(is.data.frame(x))
  vap_lgl(x, function(i) {
    all(is_blank(i, na_blank = na_blank, ws = ws))
  },
  .nm = names
  )
}
