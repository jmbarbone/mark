#' Handlers
#'
#' Catch and report handlers
#'
#' @details
#' These functions can be used to catch whether an evaluation will return an
#'   error or warning without raising.
#'
#' @returns
#' The has_* functions will return TRUE/FALSE for if the handler is found in
#'   the execution of the code.
#' The get_* functions provide the text of the message
#'
#' @param x A vector
#' @param FUN A function
#' @param .null Logical, if `FALSE` will drop `NULL` results (for `get_*()`)
#'
#' @examples
#' has_warning(c(1, "no"), as.integer)
#' #     1    no
#' # FALSE  TRUE
#'
#' get_warning(c(1, "no"), as.integer)
#' # drop NULLs
#' get_warning(c(1, "no"), as.integer, .null = FALSE)
#'
#' foo <- function(x) {
#'   stopifnot(x > 0)
#'   x
#' }
#'
#' has_error(c(1, 0, 2), foo)
#' #     1     0     2
#' # FALSE  TRUE FALSE
#'
#' get_error(c(1, 0, 2), foo)
#' # drop NULLs
#' get_error(c(1, 0, 2), foo, .null = FALSE)
#'
#'
#' @export
#' @name handlers

has_warning <- function(x, FUN) {
  has_catch(x, FUN, type = "warning")
}

#' @export
#' @rdname handlers
has_error <- function(x, FUN) {
  has_catch(x, FUN, type = "error")
}

#' @export
#' @rdname handlers
get_warning <- function(x, FUN, .null = TRUE) {
  get_catch(x, FUN, type = "warning", null = .null)
}

#' @export
#' @rdname handlers
get_error <- function(x, FUN, .null = TRUE) {
  get_catch(x, FUN, type = "error", null = .null)
}

has_catch <- function(x, FUN, type) {
  res <- lapply(x, catch(FUN))
  out <- vap_lgl(res, function(.x) !is.null(.x[[type]]))
  set_names(out, x)
}

get_catch <- function(x, FUN, type, null = TRUE) {
  res <- lapply(x, catch(FUN))
  out <- lapply(res, function(.x) .x[[type]])
  out <- set_names(out, x)

  if (!null) {
    out <- remove_null(out)
  }

  out
}

catch <- function(FUN) {
  function(...) {
    tryCatch(
      list(
        result = FUN(...),
        warning = NULL,
        error = NULL
      ),
      error = function(e) {
        list(
          result = NULL,
          error = e$message,
          warning = NULL
        )
      },
      warning = function(e) {
        list(
          result = NULL,
          error = NULL,
          warning = e$message
        )
      },
      interrupt = function(e) {
        stop("catch interrupted", call. = FALSE)
      }
    )
  }
}


#' Muffle
#'
#' Suppress messages and warnings
#'
#' @details
#' `muffle()` and `wuffle()` are aliases for [base::suppressMessages()]
#'   and [base::suppressWarnings()], respectively, except the names are shorter
#'   and therefore quicker to write.
#'
#' @param expr An expression to be evaluated
#' @param ... Additional arguments passed to [base::suppressMessages()] or
#'   [base::suppressWarnings()]
#' @export
muffle <- function(expr, ...) {
  suppressMessages(expr, ...)
}

#' @rdname muffle
#' @export
wuffle <- function(expr, ...) {
  suppressWarnings(expr, ...)
}