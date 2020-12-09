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
#' @export
not_available <- function(type = "logical", length = 0L) {
  if (is.character(type)) {
      type <- get_not_available(type)
  }

  type[0][NA][0:length]
}

#' @rdname not_available
#' @export
set_not_available <- function(type, value) {
  assign(type, value, envir = .na_env)
}

get_not_available <- function(type) {
  tryCatch(
    get(type, envir = .na_env),
    error = function(e) {
      msg <- paste0('"', type, '" not found\n',
                    "Can be set with ",
                    "`jordan::set_not_available(", type, ", value = .)`")
      stop(msg, call. = FALSE)
    }
  )
}

create_na_env <- function(env = parent.frame()) {
  .na_env <- new.env()
  assign("logical", logical(), envir = .na_env)
  assign("character", character(), envir = .na_env)
  assign("integer", integer(), envir = .na_env)
  assign("double", double(), envir = .na_env)
  assign("numeric", numeric(), envir = .na_env)
  assign("date", as.Date(NA), envir = .na_env)
  assign("POSIXct", as.POSIXct(NA), envir = .na_env)
  assign("POSIXlt", as.POSIXlt(NA)[[1]], envir = .na_env)
  assign(".na_env", .na_env, envir = env)
}

create_na_env()

#' @export
#' @rdname not_available
NA_Date_ <- not_available("date", 1L)

#' @export
#' @rdname not_available
NA_POSIXct_ <- not_available("POSIXct", 1L)

#' @export
#' @rdname not_available
NA_POSIXlt_ <- not_available("POSIXlt", 1L)
