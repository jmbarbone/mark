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
