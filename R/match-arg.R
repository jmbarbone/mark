#' Match arguments
#'
#' Match arguments
#'
#' @description
#' This function is essentially a clear version of [base::match.arg()] which
#'   produces a cleaner warning message and does not restrict the `table` param
#'   to `character` vectors only.
#'
#' @param x An argument
#' @param table A table of choices
#' @return A single value from `x` matched on `table`
#' @export
#'
#' @seealso [mark::match_param()]
#'
#' @examples
#' x <- c("apple", "banana", "orange")
#' match_arg("b", x)
#'
#' # Produces error
#' try(match_arg("pear", x))
#'
#' foo <- function(x, op = c(1, 2, 3)) {
#'   op <- match_arg(op)
#'   x / op
#' }
#'
#' foo(10, 3)
#'
#' # Error
#' try(foo(1, 0))

match_arg <- function(x, table) {
  if (is.null(x)) {
    return(NULL)
  }

  if (missing(table)) {
    sp <- sys.parent()
    args <- formals(sys.function(sp))
    table <- eval(args[[as.character(substitute(x))]], envir = sys.frame(sp))
  }

  out <- table[pmatch(x[1], table, nomatch = 0L, duplicates.ok = FALSE)]

  if (!length(out)) {
    stop(as.character(substitute(x)), ": \'", x, "\' did not match any of the following:\n\   '",
         collapse0(table, sep = "\', \'"), "\'",
         call. = FALSE)
  }
  out
}

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
#' @param null If `TRUE` allows `NULL` to be passed a `param`
#' @return A single value from `param` matched on `choices`
#'
#' @seealso [mark::match_arg()]
#'
#' @export
match_param <- function(param, choices, null = TRUE) {
  if (is.null(param)) {
    if (null) return(NULL)
    stop("match_param() requires non-NULL params", call. = FALSE)
  }

  param_c <- charexpr(substitute(param))

  if (missing(choices)) {
    parent <- sys.parent()
    forms <- formals(sys.function(parent))
    choices <- eval(forms[[param_c]], envir = parent)
  }

  res <- choices[match(param[1], choices, nomatch = 0L)[1]]
  ocall <- outer_call()

  if (no_length(res)) {

    if (is_length0(param)) {
      param <- deparse(param)
    }

    stop(sprintf(
      '`match_param(%s)` failed in `%s`:\n  `%s` [%s] must be one of the following: "%s"',
      param_c,
      ocall,
      param_c,
      param,
      collapse0(choices, sep = '", "')
    ),
    call. = FALSE)
  }

  res
}
