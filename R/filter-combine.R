#' Filter combine
#'
#' Filters a data frame then binds the result with it (with names).
#'
#' @param x A data.frame
#' @inheritDotParams dplyr::filter -.data -.preserve
#' @param .id When .id is supplied, a new column of identifiers is created to link each row to its original data frame.
#' The labels are taken from `.names`
#' @param .names vector of characters for the
#'
#' @importFrom rlang enquos
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @seealso [dplyr::filter()] and [dplyr::bind_rows()]

filter_combine <- function(x, ..., .id, .names) {
  stopifnot(length(.names) <= 2)
  dots <- enquos(...)
  xy <- list(x, filter(x, !!!dots))
  names(xy) <- .names
  bind_rows(xy, .id = .id)
}
