#' Function within
#'
#' Returns the function call you are within
#'
#' @param n The number of calls to move out from
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

#' Require namespace
#'
#' A wrapped requireNamespace
#'
#' @param namespace The name of a package/namespace
#' @export
#' @examples
#' foo <- function() {
#'   require_namespace("bad_package")
#'   1
#' }
#'
#' \dontrun{
#' require_namespace("bad_package")
#' foo()
#' }
#'
#' @references http://r-pkgs.had.co.nz/description.html
require_namespace <- function(namespace) {
  of <- outer_fun()
  of <- if (!is.na(of)) {
    sprintf(" for `%s` to work", of)
  }

  if (!rn(namespace)) {
    stop("Package `", namespace, "` is required", of, ".",
         call. = FALSE)
  }
}

rn <- function(namespace) {
  requireNamespace(namespace, quietly = TRUE)
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
#' @export
quiet_stop <- function() {
  op <- options()
  options(show.error.messages = FALSE)
  on.exit(options(op), add = TRUE)
  stop()
}
