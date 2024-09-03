#' Function within
#'
#' Returns the function call you are within
#'
#' @param n The number of calls to move out from
#' @return The string of the call/function
#'
#' @export
within_call <- function() {
  s <- sys.call(-1)
  charexpr(s)
}

#' @rdname within_call
#' @export
within_fun <- function() {
  s <- sys.call(-1)
  as.character(s)[1]
}

#' @rdname within_call
#' @export
outer_call <- function(n = 0) {
  s <- sys.call(-2 - n)
  charexpr(s)
}

#' @rdname within_call
#' @export
outer_fun <- function(n = 0) {
  s <- sys.call(-2 - n)
  as.character(s)[1]
}

rn <- function(namespace) {
  vap_lgl(namespace, requireNamespace, quietly = TRUE)
}

#' Check if package is available
#'
#' A wrapped requireNamespace
#'
#' @param namespace One or more packages to to require.
#' @export
#' @return
#' * `require_namespace()`: None, called for side effects
#' * `package_available()`: Visibly, `TRUE` or `FALSE`
#' @export
package_available <- function(namespace) {
  vap_lgl(namespace, requireNamespace, quietly = TRUE)
}

rn_soft <- function(namespace) {
  if (!rn(namespace)) {
    quiet_stop()
  }
}

#' Quiet stop
#'
#' Quietly calls stop
#'
#' @return None, called for side effects
#' @export
quiet_stop <- function() {
  op <- options()
  options(show.error.messages = FALSE)
  on.exit(options(op), add = TRUE)
  stop()
}
