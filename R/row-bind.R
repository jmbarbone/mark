#' Row bind
#'
#' Bind a list of `data.frames`
#'
#' @param ... A list of `data.frames` to be attached to each other by row
#'
#' @returns A `data.frame` combining all the rows from `data.frame`s in `...`
#'   and all the columns, as they appear. An empty `data.frame` with `0` columns
#'   and `0` rows is returned if `...` has no length
#' @seealso [dplyr::bind_rows()] [base::rbind()]
#' @examples
#' row_bind(
#'   quick_dfl(a = 1:2, b = 1:2),
#'   quick_dfl(a = 3:4, c = 3:4),
#'   quick_dfl(d = 5:6, b = 5:6),
#'   quick_dfl(e = 0),
#' )
#' @export
row_bind <- function(...) {
  ls <- list0(...)

  if (!length(ls)) {
    return(quick_df(NULL))
  }

  if (!all(vap_lgl(ls, is.data.frame))) {
    stop(cond_row_bind_dataframes())
  }

  if (length(ls) == 1) {
    return(ls[[1]])
  }

  col_nms <- lapply(ls, names)
  all_names <- unique(unlist(col_nms, use.names = FALSE))

  res <- rep(list(NULL), length(all_names))
  names(res) <- all_names

  for (i in seq_along(ls)) {
    cn <- col_nms[[i]]

    for (col in cn) {
      res[[col]] <- c(res[[col]], ls[[i]][[col]])
    }

    n <- nrow(ls[[i]])
    for (col in setdiff(all_names, cn)) {
      res[[col]] <- c(res[[col]], rep(NA, n))
    }
  }

  mark::quick_df(res)
}

# conditions --------------------------------------------------------------

cond_row_bind_dataframes <- function() {
  new_condition("... must only be data.frames", "row_bind_dataframes")
}
