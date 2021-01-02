#' To row names
#'
#' Converts a column to row names
#'
#'
#'
#' @param data A data.frame
#' @param row_names The numeric position of the column.
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

to_row_names <- function(data, row_names = 1L)
{
  row_names0 <- row_names
  if (length(row_names) != 1) {
    stop("`row_names` must be a single element vector", call. = FALSE)
  }

  if (is.character(row_names)) {
    row_names <- match(row_names, colnames(data), nomatch = NA_integer_)
  }

  if (is.na(row_names)) {
    stop("`row_names` of `", row_names0, "` is invalid", call. = FALSE)
  }

  attr(data, "row.names") <- data[[row_names]]
  data[, -row_names, drop = FALSE]
}


#' Vector to data.frame
#'
#' Transforms a vector (named) to a data.frame
#'
#' @param x A vector of values.
#' @param name,value Character strings for the name and value columns
#' @param show_NA Logical
#' @export

vector2df <- function(x, name = "name", value = "value", show_NA = FALSE) {
  stopifnot("`x` must be a vector" = is.vector(x))
  nm <- names(x)
  ln <- length(x)

  if (is.null(nm)) {
    nm <- character(ln)
  }

  if (show_NA) {
    nm[nm == ""] <- NA_character_
  }

  structure(list(v1 = nm,
                 v2 = unname(x)),
            class = "data.frame",
            row.names = c(NA_integer_, ln),
            .Names = c(name, value))
}


#' List to data.frame
#'
#' Converts a list object into a data.frame
#'
#' @details
#' Unlike [base::list2DF()], `list2df()` tries to format the data.frame by using
#'   the names of the list as values rather than variables.  This creates a
#'   longer form list that may be more tidy.
#'
#' @param x A (preferably) named `list` with any number of values
#' @param name,value Names of the new key and value columns, respectively
#' @param show_NA Logical; if FALSE elements without names will be listed as "";
#'   otherwise as NA
#' @param warn Logical; if TRUE will show a warning when
#'
#' @return a `data.frame` object with columns "name" and "value" for the names
#' of the `list` and the values in each
#' @export
#' @examples
#' \dontrun{
#' x <- list(a = 1, b = 2:4, c = letters[10:20])
#' list2df(x, "col1", "col2", force = TRUE)
#' # contrast with `base::list2DF()` w
#' list2DF(x)
#' }

list2df <- function(x, name = "name", value = "value", show_NA = FALSE, warn = TRUE) {
  stopifnot("`x` must be a list" = is.list(x))

  cl <- lapply(x, class)
  n_cl <- length(unique(cl))

  if (n_cl > 1 & warn) {
    warning("Not all values are the same class", call. = FALSE)
    if (any(c("character", "factor") %in% cl)) {
      warning("Values converted to character", call. = FALSE)
    }
  }

  ulist <- unlist(x, use.names = FALSE)
  ln <- length(ulist)
  nm <- rep(names(x), vap_int(x, length))

  if (is.null(nm)) {
    nm <- character(ln)
  }

  if (show_NA) {
    nm[nm == ""] <- NA_character_
  }

  structure(list(name = nm,
                 value = unname(ulist)),
            class = "data.frame",
            row.names = c(NA_integer_, ln),
            .Names = c(name, value))
}


#' Data frame transpose
#'
#' Transposes a data.frame
#'
#' @param x A data.frame
#' @param id The identification column number
#' @param name The name of the identification column
#' @export

t_df <- function(x, id = 1L, name = ".id") {
  temp <- data.frame(t(x[-id]), stringsAsFactors = FALSE, row.names = NULL)
  colnames(temp) <- x[[id]]
  temp$`.id` <- colnames(x)[-id]
  temp[c(name, paste(colnames(temp)[-ncol(temp)]))]
}
