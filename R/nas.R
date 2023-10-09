#' Remove NA
#'
#' Remove NAs from a vector
#'
#' @details
#' `remove_na.factor` will remove `NA` values as identified by the `levels()`
#'   or by the integer value of the level.  `factors` are recreated with all
#'   `NA` values and, if present, the `NA` `level` removed.
#'
#' @param x A vector of values
#' @returns
#'   `x` without values where `is.na(x)` is `TRUE`
#'   For factors, a new factor (`ordered` if `is.ordered(x)`)
#' @export
#' @examples
#' remove_na(c(4, 1, 2, NA, 4, NA, 3, 2))
#'
#' # removes based on levels
#' remove_na(fact(c("b", NA, "a", "c")))
#'
#' # removes based on values
#' x <- as_ordered(c("b", "d", "a", "c"))
#' x[2:3] <- NA
#' str(remove_na(x))
remove_na <- function(x) {
  check_is_vector(x, "any")
  UseMethod("remove_na", x)
}

#' @rdname remove_na
#' @export
remove_na.default <- function(x) {
  if (anyNA(x)) x[!is.na(x)] else x
}

#' @rdname remove_na
#' @export
remove_na.list <- function(x) {
  lapply(x, remove_na)
}

#' @rdname remove_na
#' @export
remove_na.factor <- function(x) {
  x <- x[!is.na(x)]
  levels(x) <- remove_na(levels(x))
  x
}

#' @rdname remove_na
#' @export
remove_na.fact <- function(x) {
  x <- fact_na(x, remove = TRUE)
  at <- attributes(x)
  x <- x[!is.na(x)]
  attributes(x) <- at
  x
}

#' Omit NA values
#'
#' @param x A vector of values
#' @return `x` which `NA` values removes and two attributes of `integers`: `na`
#'   which is the position of `NA` values, and `valid` for the position of
#'   non-`NA` values; empty positions reported as `integer(0)`
#' @examples
#' # Like stats::na.omit but always provides
#' x <- letters[1:5]
#' omit_na(x)
#' x[c(3, 5)] <- NA
#' omit_na(x)
#'
#' @export
omit_na <- function(x) {
  if (anyNA(x)) {
    nas <- is.na(x)
    struct(x, class = class(x), na = which(nas), valid = which(!nas))
  } else {
    struct(x, class = class(x), na = integer(), valid = seq_along(x))
  }
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
  stopifnot(inherits(x, "list"))
  x[!vap_lgl(x, is.null)]
}

#' Selecting NA columns
#'
#' Select or remove columns that are entirely NA
#'
#' @param x A `data.frame`
#' @param names Logical, if `TRUE` (default) will return column names as names
#'   of vector
#'
#' @returns
#' * `select_na_cols()` `x` with only columns that are all `NA`
#' * `remove_na_cols()` `x` without columns of only `NA`
#' * `is_na_cols()` a logical vector: `TRUE` all rows of column are `NA`,
#'  otherwise `FALSE`
#' @name na_cols
NULL

#' @rdname na_cols
#' @export
select_na_cols <- function(x) {
  x[, is_na_cols(x), drop = FALSE]
}

#' @rdname na_cols
#' @export
remove_na_cols <- function(x) {
  x[, !is_na_cols(x), drop = FALSE]
}

#' @rdname na_cols
#' @export
is_na_cols <- function(x, names = TRUE) {
  stopifnot(is.data.frame(x))
  vap_lgl(x, function(xx) all(is.na(xx)), .nm = names)
}

#' Table NA values
#'
#' Tables out whether data are NAs are not
#'
#' @details
#' All data are checked with `is.na()` and the resulting `TRUE` or `FALSE` is
#'   are tabulated.
#'
#' @inherit base::table
#' @param .list Logical, if `TRUE` and `...` is a `list`, will c
#' @export
#' @examples
#' x <- list(
#'   a = c(1, 2, NA, 3),
#'   b = c("A", NA, "B", "C"),
#'   c = as.Date(c("2020-01-02", NA, NA, "2020-03-02"))
#' )
#' tableNA(x) # entire list
#' tableNA(x, .list = TRUE) # counts for each
#' tableNA(x[1], x[2])
#' tableNA(x[1], x[2], x[3]) # equivalent ot tableNA(x, .list = TRUE)

tableNA <- function(..., .list = FALSE) { # nolint: object_name_linter
  ls <- if (.list) {
    as.list(...)
  } else {
    rlang::list2(...)
  }

  if (is.null(names(ls))) {
    names(ls) <- as.character(sys.call())[-1]
  }

  out <- table(lapply(ls, function(x) mark::fact(is.na(x))))
  dn <- rep(list(c(TRUE, FALSE)), length(ls))
  names(dn) <- names(ls)
  dimnames(out) <- dn
  out
}
