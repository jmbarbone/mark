#' echo
#'
#' Run expressions with logging outputs
#'
#' @details
#' Function is deprecated.  Use `echo::echo()` instead.
#'
#' @param exprs Expressions
#' @param to Output locations
#' @param msg If `FALSE` does not print results from `message()`
#' @export
#' @examples
#' try(echo({
#'   1 + 1
#'   Sys.sleep(2)
#'   head(mtcars)
#'   message(1)
#'   warning(2)
#'   stop(3)
#' }))
#'
#' if (package_available("echo")) {
#'   try(echo::echo({
#'     1 + 1
#'     Sys.sleep(2)
#'     print(head(mtcars))
#'     message(1)
#'     warning(2)
#'     stop(3)
#'   },
#'   level = 0
#'   ))
#' }
echo <- function(exprs, to = stdout(), msg = TRUE) {
  .Deprecated("echo::echo()")
  env <- environment()
  exprs <- as.list(substitute(exprs))[-1]
  time <- function() paste0("[", format(Sys.time(), tz = "UTC"), "] ")

  cat0 <- function(..., sep = "") {
    cat(..., sep = sep, file = to)
  }

  catln <- function(...) {
    cat0(..., "\n")
  }

  # TODO add functions for other controls
  msg <- if (isTRUE(msg)) {
    function(x) cat0(paste0(time(), "[MSG] #> ", conditionMessage(x)))
  } else {
    function(x) invisible()
  }

  op <- options(width = max(getOption("width") - 37, 30))
  on.exit(options(op))

  res <- NULL

  for (exp in exprs) {
    cat0(time(), "[EXP] ")
    dep <- deparse1(exp)
    catln(dep)

    res <- tryCatch(
      eval(as.expression(exp), envir = env),
      error = function(e) {
        catln(paste0(time(), "[ERR] #> ", conditionMessage(e)))
        stop("Error in ", dep, "\n  ", conditionMessage(e), call. = FALSE)
      },
      warning = function(e) {
        catln(paste0(time(), "[WRN] #> ", conditionMessage(e)))
        tryInvokeRestart("muffleWarning")
      },
      message = function(e) {
        msg(e)
        tryInvokeRestart("muffleMessage")
      }
    )

    if (is.null(res)) {
      utils::flush.console()
    } else {
      catln(
        paste0(time(), "[OUT] #> ", utils::capture.output(res), collapse = "\n")
      )
    }
  }

  invisible(res)
}
