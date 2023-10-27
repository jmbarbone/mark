
#' Try an expression a set number of times
#'
#' @param expr expression to evaluate
#' @param n number of attempts until error
#' @param silent whether to suppress warnings
#' @return result of `expr`
#' @export
#' @examples
#' foo <- function() stop("I added an error")
#' try(tryn(n = 10, foo()))
tryn <- function(expr, n = 10, silent = TRUE) {
  e <- parent.frame()
  attempt <- 0
  expr <- as.expression(substitute(expr))
  while (
    inherits(res <- try(eval(expr, envir = e), silent = silent), "try-error")
  ) {
    attempt <- attempt + 1
    if (attempt == n) {
      cond <- attr(res, "condition")
      msg <- paste0(
        "tryn() failed: maximum attempts reached: ",
        attempt, "\n",
        "Error in ",
        as.character(as.expression(cond[["call"]])), " : ",
        cond[["message"]]
      )
      stop(simpleError(msg))
    }
  }

  res
}
