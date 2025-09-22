#' To row names
#'
#' Converts a column to row names
#'
#' @param data A data.frame
#' @param row_names The numeric position of the column.
#' @return A `data.frame`
#' @examples
#'
#' x <- data.frame(
#'   a = 1:4,
#'   b = letters[1:4]
#' )
#'
#' to_row_names(x)
#' to_row_names(x, "b")
#' @export

to_row_names <- function(data, row_names = 1L) {
  row_names0 <- row_names

  if (length(row_names) != 1) {
    stop(to_row_names_single())
  }

  if (is.character(row_names)) {
    row_names <- match(row_names, colnames(data), nomatch = NA_integer_)
  }

  if (is.na(row_names)) {
    stop(to_row_names_na(row_names0))
  }

  x <- data[[row_names]]

  if (!is.integer(x)) {
    x <- as.character(x)
  }

  attr(data, "row.names") <- x # nolint: object_name_linter.
  data[, -row_names, drop = FALSE]
}

#' Vector to data.frame
#'
#' Transforms a vector (named) to a data.frame
#'
#' @param x A vector of values.
#' @param name,value Character strings for the name and value columns
#' @return A `data.frame` with `name` (optional) and `value` columns
#' @export

vector2df <- function(x, name = "name", value = "value") {
  stopifnot(!is.list(x))
  ls <- list(names(x) %||% rep(NA, length(x)), remove_names(x))
  ls <- ls[!vap_lgl(list(name, value), is.null)]
  names(ls) <- c(name, value)
  quick_df(ls)
}

#' List to data.frame
#'
#' Converts a list object into a data.frame
#'
#' @details
#' Unlike `base::list2DF()`, `list2df()` tries to format the data.frame by using
#'   the names of the list as values rather than variables.  This creates a
#'   longer form list that may be more tidy.
#'
#' @param x A (preferably) named `list` with any number of values
#' @param name,value Names of the new key and value columns, respectively
#' @param warn Logical; if TRUE will show a warning when
#'
#' @return a `data.frame` object with columns `"name"` and `"value"` for the
#'   names of the `list` and the values in each
#' @export
#'
#' @examples
#' x <- list(a = 1, b = 2:4, c = letters[10:20], "unnamed", "unnamed2")
#' list2df(x, "col1", "col2", warn = FALSE)
#'
#' if (getRversion() >= as.package_version('4.0')) {
#' # contrast with `base::list2DF()` and `base::as.data.frame()`
#'   x <- list(a = 1:3, b = 2:4, c = letters[10:12])
#'   list2df(x, warn = FALSE)
#'   list2DF(x)
#'   as.data.frame(x)
#' }

# nolint next: object_name_linter.
list2df <- function(x, name = "name", value = "value", warn = TRUE) {
  stopifnot(is.list(x))

  cl <- lapply(x, class)
  n_cl <- length(unique(cl))

  if (n_cl > 1L && warn) {
    warning(list2df_classes(cl))
  }

  ulist <- unlist(x, use.names = FALSE)
  nm <- names(x)
  blanks <- nm == ""
  nm[blanks] <- which(blanks)

  out <- quick_df(
    list(name = rep(make.unique(nm), lengths(x)),
         value = unname(ulist))
  )

  names(out) <- c(name, value)
  out
}

# base::list2DF() -- but this wasn't introduced until 4.0.0
# And an update prevents recycling
list2df2 <- function(x = list(), nrow = NULL) {
  stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)

  if (n <- length(x)) {
    if (is.null(nrow)) {
      nrow <- max(lengths(x), 0L)
    }

    x <- lapply(x, rep_len, nrow)
  } else if (is.null(nrow)) {
    nrow <- 0L
  }

  if (is.null(names(x))) {
    names(x) <- character(n)
  }

  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(nrow) # nolint: object_name_linter.
  x
}

#' Data frame transpose
#'
#' Transposes a data.frame as a data.frame
#'
#' @description
#' This transposes a data.frame with `t()` but transforms back into a data.frame
#'   with column and row names cleaned up.  Because the data types may be mixed
#'   and reduced to characters, this may only be useful for a visual viewing of
#'   the data.frame.
#'
#' @param x A data.frame
#' @return A transposed `data.frame` with columns (`"colname"`, `"row_1"`, ...,
#'   for each row in `x`.
#'
#' @examples
#' x <- data.frame(col_a = Sys.Date() + 1:5, col_b = letters[1:5], col_c = 1:5)
#' t_df(x)
#' @export

t_df <- function(x) {
  stopifnot(is.data.frame(x))

  out <- as.data.frame(
    t(x),
    stringsAsFactors = FALSE,
    optional = TRUE,
    make.names = FALSE
  )

  colnames(out) <- paste0("row_", seq_len(nrow(x)))
  rn_to_col(out, "colname")
}

rn_to_col <- function(data, name = "row.name") {
  stopifnot(is.data.frame(data))
  n <- length(data) + 1
  data[[n]] <- attr(data, "row.names")
  data <- reset_rownames(data)
  colnames(data)[n] <- name
  data[, c(n, seq_len(n - 1)), drop = FALSE]
}

#' Complete cases
#'
#' Return completed cases of a data.frame
#'
#' @param data A data.frame
#' @param cols Colnames or numbers to remove `NA` values from; `NULL` (default)
#'   will use all columns
#' @param invert Logical, if `TRUE` will return incomplete cases
#' @return A `data.frame`
#'
#' @examples
#' x <- data.frame(
#'   a = 1:5,
#'   b = c(1, NA, 3, 4, 5),
#'   c = c(1, NA, NA, 4, 5)
#' )
#'
#' complete_cases(x)
#' complete_cases(x, invert = TRUE) # returns the incomplete rows
#' complete_cases(x, "a")
#' complete_cases(x, "b")
#' complete_cases(x, "c")
#' @export
complete_cases <- function(data, cols = NULL, invert = FALSE) {
  stopifnot(is.data.frame(data))
  ds <- dim(data)

  if (ds[1L] == 0L || ds[2L] == 0L) {
    stop(complete_cases_dims())
  }

  x <- data[, cols %||% 1:ds[2L], drop = FALSE]
  cc <- stats::complete.cases(x[, vap_lgl(x, anyNA), drop = FALSE])

  if (invert) {
    cc <- !cc
  }

  reset_rownames(data[cc, ])
}

#' Unique rows
#'
#' Drops duplicated rows
#'
#' @param data A `data.frame`
#' @param cols Columns to compare against; when `NULL` selects all columns
#' @param from_last When `TRUE` returns the last row containing duplicates,
#'   rather than the first
#' @param invert If `TRUE` returns the duplicated rows
#' @returns `data` will duplicates removes
#' @examples
#' df <- quick_dfl(
#'   i = 1:4,
#'   a = rep(1:2, 2L),
#'   b = rep("a", 4L),
#' )
#'
#' unique_rows(df, 2:3)
#' unique_rows(df, c("a", "b"), from_last = TRUE, invert = TRUE)
#' @export
unique_rows <- function(data, cols = NULL, from_last = FALSE, invert = FALSE) {
  stopifnot(is.data.frame(data))
  cn <- names(data)
  cols <- cols %||% cn

  if (is.numeric(cols)) {
    cols <- cn[cols]
  }

  keep <- duplicated(data[, cols, drop = FALSE], fromLast = from_last)

  if (!invert) {
    keep <- !keep
  }

  reset_rownames(data[keep, ])
}


reset_rownames <- function(data, n = nrow(data)) {
  attr(data, "row.names") <- .set_row_names(n)
  data
}


# conditions --------------------------------------------------------------

to_row_names_single := condition(
  "row_names must be a single element vector",
  type = "error",
  exports = "to_row_names"
)

to_row_names_na := condition(
  function(x) sprintf("`row_names` of `%s` is invalid", x),
  type = "error",
  exports = "to_row_names"
)


list2df_classes := condition(
  function(x) {
    ngettext(
      any(c("character", "factor") %in% x),
      "Not all values are the same class: converting to character",
      "Not all values are the same class"
    )
  },
  type = "warning",
  exports = "list2df"
)

complete_cases_dims := condition(
  "`data` must have at least 1 row and 1 column",
  type = "error",
  exports = "complete_cases"
)
