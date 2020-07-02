#' Vector to data.frame
#'
#' Transforms a vector (named) to a data.frame
#'
#' @param vec A vector of values.
#' @param name,value Character strings for the name and value columns
#' @param show_NA Logical
#' @export

vector2df <- function(vec, name = "name", value = "value", show_NA = FALSE) {
  if (!is.vector(vec)) stop("Object is not a vector.", call. = FALSE)
  vector_names <- names(vec)

  if (show_NA) {
    vector_names[vector_names == ""] <- NA_character_
  } else if (is.null(vector_names) || any(vector_names == "")) {
    warning("Name missing from vector.", call. = FALSE)
  }

  out <- data.frame(name = vector_names,
                    value = unname(vec),
                    stringsAsFactors = FALSE,
                    fix.empty.names = FALSE,
                    check.names = FALSE)
  colnames(out) <- c(name, value)
  out
}
