#' Match params
#'
#' Param matching for an argument
#'
#' @description
#' Much like [base::match.arg()] with a few key differences:
#' * Will not perform partial matching
#' * Will not return error messages with ugly quotation marks
#'
#' @param param The parameter
#' @param choices The available choices
#'
#' @export
match_param <- function(param, choices) {
  stopifnot(!is.null(param))

  param_c <- as.character(substitute(param))

  if (missing(choices)) {
    parent <- sys.parent()
    forms <- formals(sys.function(parent))
    choices <- eval(forms[[param_c]], envir = parent)
  }

  res <- choices[match(param[1], choices, nomatch = 0L)[1]]

  if (length(res) == 0) {
    stop(sprintf(
      '`match_param(%s)` failed in `%s`:\n  `%s` must be one of the following: "%s"',
      param_c,
      within_call(),
      param_c,
      paste(choices, collapse = '", "')
    ),
    call. = FALSE)
  }

  res
}

#' Function within
#'
#' Returns the function call you are within
#'
#' @export
within_call <- function() {
  as.character(as.expression(sys.call(2)))
}

#' @rdname within_call
#' @export
within_fun <- function() {
  as.character(sys.call(2))
}

#' Require namespace
#'
#' A wrapped requireNamespace
#'
#' @param namespace The name of a package/namespace
#' @export
#' @references http://r-pkgs.had.co.nz/description.html
require_namespace <- function(namespace) {
  if (!rn(namespace)) {
    stop(paste0("Package `", namespace, "`needed for this function to work."),
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