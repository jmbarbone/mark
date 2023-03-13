#' Check options
#'
#' Checks and reports on options
#'
#' @description
#' For each name in `x` checks the current option value and reports if there
#'   is a difference in a `message`.  This does not change the options
#'
#' @param x A named list of new options
#' @returns Invisible, a list of the current options from `options()`
#'
#' @examples
#' op <- options()
#'
#' x <- list(width = -20, warning.length = 2, probably_not_a_real_option = 2)
#' checkOptions(x)
#' # pointless, but shows that no messages are given
#' identical(options(), checkOptions(options()))
#'
#' options(op)
#' @export
checkOptions <- function(x) { # nolint: object_name_linter.
  stopifnot(is.list(x))

  nm <- names(x)
  if (is.null(nm) || any(nm == "")) {
    stop(cond_check_options_names())
  }

  msg <- NULL
  op <- options()
  for (i in seq_along(x)) {
    go <- op[[nm[i]]]

    if (is.null(go)) {
      next
    }

    if (!identical(x[[i]], go)) {
      if (is.null(msg)) {
        msg <- "Option(s) updated :"
      }

      msg <- c(msg, sprintf(
        '\n "%s"\n   old : %s\n   new : %s',
        nm[i],
        go,
        x[[i]]
      ))
    }
  }

  if (!is.null(msg)) {
    message(msg)
  }

  invisible(op)
}

# conditions --------------------------------------------------------------

cond_check_options_names <- function() {
  new_condition("All options must be named", "check_options_names")
}

# terminal line
