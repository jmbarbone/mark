#' Make not available
#'
#' Create NA vectors
#'
#' @details
#' If length is a text it will search for an appropriate match.
#'
#' @param type Type of NA (see details)
#' @param length Length of the vector
#' @param value A value to return in `not_available()`
#'
#' @return A vector of `NA` values
#' @examples
#' x <- not_available("Date", 3)
#' x
#' class(x)
#'
#' @export
not_available <- function(type = "logical", length = 0L) {
  if (is.character(type)) {
    type <- get_not_available(type)
  }

  rep(type[0][NA], length)
}

#' @rdname not_available
#' @export
set_not_available <- function(type, value) {
  ls <- get_na_list()
  ls[[type]] <- value
  options(mark.na_list = ls)
}

get_not_available <- function(type = NULL) {

  if (is.null(type)) {
    return(get_na_list())
  }

  out <- get_na_list()[[type]]

  if (is.null(out)) {
    stop(
      '"', type, '" not found\n',
      "Can be set with `mark::set_not_available(", type, ", value = .)`",
      call. = FALSE
    )
  }

  if (is.function(out) || is.call(out)) {
    stop("type is not valid", call. = FALSE)
  }

  out
}

get_na_list <- function() {
  ls <- getOption("mark.na_list", list())

  if (identical(ls, list())) {
    options(mark.na_list = na_list)
    ls <- na_list
  }

  ls
}

#' @export
#' @rdname not_available
NA_Date_ <- not_available("Date", 1L)

#' @export
#' @rdname not_available
NA_POSIXct_ <- not_available("POSIXct", 1L)

#' @export
#' @rdname not_available
NA_POSIXlt_ <- not_available("POSIXlt", 1L)
