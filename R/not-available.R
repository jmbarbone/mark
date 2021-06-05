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

  type[0][NA][0:length]
}

#' @rdname not_available
#' @export
set_not_available <- function(type, value) {
  assign(type, value, envir = .na_env)
}

get_not_available <- function(type) {
  if (!exists(".na_env", mode = "environment")) {
    # Temporarilty creates
    create_na_env()
  }

  tryCatch(
    get(type, envir = .na_env),
    error = function(e) {
      msg <- paste0('"', type, '" not found\n',
                    "Can be set with ",
                    "`mark::set_not_available(", type, ", value = .)`")
      stop(msg, call. = FALSE)
    },
    finally = function(x) {
      if (!is.object(x) || is.function(x) || is.call(x)) {
        stop("type is not valid", call. = FALSE)
      }
    }
  )
}

create_na_env <- function(env = parent.frame()) {
  .na_env <- new.env()

  list2env(
    list(logical = logical(),
         character = character(),
         integer = integer(),
         double = double(),
         numeric = numeric(),
         Date = as.Date(NA),
         POSIXct = as.POSIXct(NA),
         POSIXlt = as.POSIXlt(NA)[[1]]),
    envir = .na_env
  )

  assign(".na_env", .na_env, envir = env)
}

# Create the environment
create_na_env()

#' @export
#' @rdname not_available
NA_Date_ <- not_available("Date", 1L)

#' @export
#' @rdname not_available
NA_POSIXct_ <- not_available("POSIXct", 1L)

#' @export
#' @rdname not_available
NA_POSIXlt_ <- not_available("POSIXlt", 1L)
