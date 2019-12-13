#' Vector to data.frame
#'
#' Transforms a vector (named) to a data.frame
#'
#' @param .vector A vector of values.
#' @param show_NA Logical.
#' @export

vector2df <- function(.vector, show_NA = F)
{
  if(!is.vector(.vector)) stop("Object is not a vector!", call. = F)
  vector_names <- names(.vector)
  if(show_NA) vector_names[vector_names == ""] <- NA else if(is.null(vector_names) || any(vector_names == "")) warning("Name missing from vector.", call. = F)

  data.frame(
    names = vector_names,
    values = unname(.vector),
    stringsAsFactors = F
  )
}
