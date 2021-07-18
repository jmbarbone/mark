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
#' x <- c(4L, 1L, 2L, 1L, 4L, 1L, 3L, 2L)
#' remove_na(x)
#'
#' # removes based on levels
#' x <- structure(x, levels = c("b", NA, "a", "c"), class = "factor")
#' remove_na(x)
#'
#' # removes based on values
#' levels(x) <- c("b", "d", "a", "c")
#' class(x) <- c("ordered", "factor")
#' x[2:3] <- NA
#' str(remove_na(x))
remove_na <- function(x) {
  check_is_vector(x, "any")
  UseMethod("remove_na", x)
}

#' @rdname remove_na
#' @export
remove_na.default <- function(x) {
  x[!is.na(x)]
}

#' @rdname remove_na
#' @export
remove_na.list <- function(x) {
  lapply(x, remove_na)
}

#' @rdname remove_na
#' @export
remove_na.factor <- function(x) {
  lvls <- levels(x)
  na_levels <- is.na(lvls)

  out <- x[!is.na(x)]
  out <- as.integer(out)

  if (any(na_levels)) {
    which_na_level <- which(na_levels)
    out <- out[out != which_na_level]

    if (which_na_level != length(lvls)) {
      out <- match(out, unique(out))
    }
  }

  levels(out) <- lvls[!na_levels]
  class(out) <- c(if (is.ordered(x)) "ordered", "factor")
  out
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
  if (!inherits(x, "list")) {
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
#' x <- list(a = c(1, 2, NA, 3), b = c("A", NA, "B", "C"), c = as.Date(c("2020-01-02", NA, NA, "2020-03-02")))
#' tableNA(x) # entire list
#' tableNA(x, .list = TRUE) # counts for each
#' tableNA(x[1], x[2])
#' tableNA(x[1], x[2], x[3]) # equivalent ot tableNA(x, .list = TRUE)

# TODO add tests for tableNA()
# TODO add tableNA() to NEWS.md

tableNA <- function(..., .list = FALSE) {
  # browser()
  # This is from table():
  # ls <- as.list(substitute(list(...)))[-1L]
  # nm <- names(ls)
  # fixup <- if (is.null(nm))
  #   seq_along(ls)
  # else nm == ""
  ls <- if (.list)
    as.list(...)
  else
    list(...)
  if (is.null(names(ls)))
    names(ls) <- as.character(sys.call())[-1]
  table(lapply(ls, function(x) mark::fact(is.na(x))))
}
