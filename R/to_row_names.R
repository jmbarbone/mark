to_row_names <- function(.df, .row_names = 1)
{
  rownames(.df) <- unlist(.df[.row_names], use.names = F)
  .df[-.row_names]
}
