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
