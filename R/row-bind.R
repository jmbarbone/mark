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
#' @export
row_bind <- function(...) {
  ls <- remove_null(if (...length() == 1) ..1 else rlang::list2(...))

  if (!length(ls)) {
    return(quick_df(NULL))
  }

  if (!all(vap_lgl(ls, is.data.frame))) {
    stop(cond_row_bind_dataframes())
  }

  names <- lapply(ls, names)
  all_names <- unique(unlist(names))

  cinds <- lapply(
    names,
    function(x, y) match(y, x, nomatch = NA_integer_),
    all_names
  )

  res <- mapply(
    function(a, b) {
      w <- which(is.na(b))

      if (length(w)) {
        a <- quick_df(set_names(insert(a, w, NA), all_names))
      }

      a
    },
    a = ls,
    b = cinds,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  # TODO use mark:::type_convert2() ?
  utils::type.convert(do.call(rbind2, res), as.is = TRUE)
}

# This may be a little faster than rbind()
rbind2 <- function(...) {
  ls <- rlang::list2(...)
  res <- list()

  for (i in seq_along(ls[[1]])) {
    res[[i]] <- Reduce(c, lapply(ls, `[[`, i))
  }

  quick_df(set_names(res, names(ls[[1]])))
}

# conditions --------------------------------------------------------------

cond_row_bind_dataframes <- function() {
  new_condition("... must only be data.frames", "row_bind_dataframes")
}
