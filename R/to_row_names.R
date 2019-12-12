#' To row names
#'
#' Converts a column to row names
#'
#' @param .df A data.frame
#' @param .row_names The numeric position of the column.
#' @export

to_row_names <- function(.df, .row_names = 1)
{
  rownames(.df) <- unlist(.df[.row_names], use.names = F)
  .df[-.row_names]
}
