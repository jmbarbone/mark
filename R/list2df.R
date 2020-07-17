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
  nm <- rep(names(x), vapply(x, length, integer(1)))

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
