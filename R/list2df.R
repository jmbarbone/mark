#' List to data.frame
#'
#' Converts a list object into a a tidy data.frame
#'
#' @param .list A (prefferably) named `list` with any number of values
#' @param show_NA Logical.  If FALSE elements without names will be listed as ""; otherwise as NA.
#' @param ... Arguments passed to `as.data.frame()`
#' @return a `data.frame` object with columns "name" and "value" for the names of the `list` and the values in each
#' @export

list2df <- function(.list, show_NA = F, ...)
{
  if(!is.list(.list)) stop("Object is not a list!", call. = F)
  classes <- sapply(.list, class, simplify = T)
  n_classes <- length(unique(classes))
  if(n_classes > 1) warning("Not all values are the same class!", call. = F)
  if(n_classes > 1 && "character" %in% classes) warning("Values converted to character.", call. = F)

  list_names <- rep(names(.list), sapply(.list, length))
  if(show_NA) list_names[list_names == ""] <- NA else if(is.null(list_names) || any(list_names == "")) warning("Name missing from list.", call. = F)

  data.frame(
    name = list_names,
    value = unlist(.list, use.names = F),
    stringsAsFactors = F)
}
