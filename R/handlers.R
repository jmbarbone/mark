#' Handlers
#'
#' Catch and report handlers
#'
#' @details
#' These functions can be used to catch whether an evaluation will return an
#'   error or warning without raising.
#'
#' @return
#' The `has_*()` functions will return `TRUE`/`FALSE` for if the handler is
#'   found in the execution of the code.
#' The `get_*()` functions provide the text of the message
#'
#' @param x A vector
#' @param FUN A function
#' @param .null Logical, if `FALSE` will drop `NULL` results (for `get_*()`)
#' @param ... Additional params passed to `FUN`
#'
#' @references
#' Function for _catching_ has been adapted from https://stackoverflow.com/a/4952908/12126576
#'
#' @examples
#' has_warning(c(1, "no"), as.integer)
#' #     1    no
#' # FALSE  TRUE
#'
#' get_warning(c(1, "no"), as.integer)
#'
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
#'
#' # drop NULLs
#' get_error(c(1, 0, 2), foo, .null = FALSE)
#' @name handlers
NULL

#' @export
#' @rdname handlers
has_warning <- function(x, FUN, ...) {
  has_catch(x, FUN, ..., type = "warning")
}

#' @export
#' @rdname handlers
has_error <- function(x, FUN, ...) {
  has_catch(x, FUN, ..., type = "error")
}

#' @export
#' @rdname handlers
has_message <- function(x, FUN, ...) {
  has_catch(x, FUN, ..., type = "message")
}

#' @export
#' @rdname handlers
get_warning <- function(x, FUN, ..., .null = TRUE) {
  get_catch(x, FUN, type = "warning", null = .null)
}

#' @export
#' @rdname handlers
get_message <- function(x, FUN, ..., .null = TRUE) {
  get_catch(x, FUN, type = "message", null = .null)
}

#' @export
#' @rdname handlers
get_error <- function(x, FUN, ..., .null = TRUE) {
  get_catch(x, FUN, type = "error", null = .null)
}

has_catch <- function(x, FUN, ..., type = c("error", "warning", "message")) {
  type <- match_param(type)
  FUN <- match.fun(FUN)
  res <- sapply(x, catch(FUN), ..., USE.NAMES = TRUE, simplify = FALSE)
  out <- vap_lgl(res, function(i) !is.null(i[[type]]))
  attr(out, "result") <- lapply(res, `[[`, "result")
  attr(out, "class") <- c("has_catch", "logical")
  set_names0(out, x)
}

#' @export
print.has_catch <- function(x, ...) {
  print(set_names0(remove_attributes(x), names(x)))
  invisible(x)
}

get_catch <- function(x, FUN, type, ..., null = TRUE) {
  res <- sapply(x, catch(FUN), ..., USE.NAMES = TRUE, simplify = FALSE)
  out <- sapply(res, function(i) i[[type]], USE.NAMES = TRUE, simplify = FALSE)
  out <- set_names0(out, x)

  if (!null) {
    out <- remove_null(out)
  }

  out
}

# Adapted from https://stackoverflow.com/a/4952908/12126576
catch <- function(FUN) {
  FUN <- match.fun(FUN)

  function(...) {
    env <- list2env(list(error = NULL, warning = NULL, message = NULL))
    res <- withCallingHandlers(
      tryCatch(
        FUN(...),
        error = function(e) {
          env$error <- c(env$error, e$message)
          NULL
        }
      ),
      warning = function(e) {
        env$warning <- c(env$warning, e$message)
        invokeRestart("muffleWarning")
      },
      message = function(e) {
        env$message <- c(env$message, e$message)
        invokeRestart("muffleMessage")
      }
    )

    list(
      result = res,
      error = env$error,
      warning = env$warning,
      message = env$message
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
#' @return The result of `expr`
#'
#' @export
muffle <- function(expr, ...) {
  suppressMessages(expr, ...)
}

#' @rdname muffle
#' @export
wuffle <- function(expr, ...) {
  suppressWarnings(expr, ...)
}
