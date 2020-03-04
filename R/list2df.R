#' List to data.frame
#'
#' Converts a list object into a a tidy data.frame
#'
#' @param list A (preferably) named `list` with any number of values
#' @param key,value Names of the new key and value columns, respectively
#' @param show_NA Logical; if FALSE elements without names will be listed as ""; otherwise as NA
#' @return a `data.frame` object with columns "name" and "value" for the names of the `list`
#'   and the values in each
#' @export
#' @examples
#' \dontrun{
#' list2df(list(a = 1, b = 2:4, c = letters[10:20]), "col1", "col2")
#' }

list2df <- function(list, key = "key", value = "value", show_NA = FALSE) {
  UseMethod("list2df", list)
}

#' @export
list2df.default <- function(list, key = "key", value = "value", show_NA = FALSE) {
  stop("Object must be class list", call. = FALSE)
}

#' @export
list2df.list <- function(list, key = "key", value = "value", show_NA = FALSE) {
  classes <- sapply(list, class, simplify = TRUE)
  n_classes <- length(unique(classes))

  if(n_classes > 1) {
    warning("Not all values are the same class", call. = FALSE)
    if(any(c("character", "factor") %in% classes)) {
      warning("Values converted to character", call. = FALSE)
    }
  }

  list_names <- rep(names(list), sapply(list, length))

  if(show_NA) {
    list_names[list_names == ""] <- NA
  }  else if(is.null(list_names) | any(list_names == "")) {
    warning("Name missing from list.", call. = FALSE)
  }

  data.frame(key = list_names,
             value = unlist(list, use.names = FALSE),
             stringsAsFactors = FALSE) %>%
    `names<-`(c(key, value))
}


# testing -------------------------------------------------------------------------------------

# length(x)
# lengths(x)
#
# for(i in seq_along(x)) {
#   magrittr::extract2(x, 1)
#   print(sapply(x[[i]], length))
# }
