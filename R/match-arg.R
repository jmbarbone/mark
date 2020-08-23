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
#' @export
#'
#' @examples
#' x <- c("apple", "banana", "orange")
#' match_arg("b", x)
#'
#' # Produces error
#' tryCatch(
#'   match_arg("pear", x),
#'   error = function(e) e,
#'   finally = invisible()
#' )
#'
#' foo <- function(x, op = c(1, 2, 3)) {
#'   op <- match_arg(op)
#'   x / op
#' }
#'
#' foo(10, 3)
#'
#' # Error
#' tryCatch(
#'   foo(1, 0),
#'   error = function(e) e,
#'   finally = invisible()
#' )

match_arg <- function(x, table) {
  if (is.null(x)) return(NULL)

  char_sub <- as.character(substitute(x))

  if (missing(table)) {
    sp <- sys.parent()
    args <- formals(sys.function(sp))
    table <- eval(args[[as.character(substitute(x))]], envir = sys.frame(sp))
  }

  out <- table[pmatch(x[1], table, nomatch = 0L, duplicates.ok = FALSE)]

  if (!length(out)) {
    stop(as.character(substitute(x)), ": \'", x, "\' did not match any of the following:\n\   '",
         paste(table, collapse = "\', \'"), "\'", call. = FALSE)
  }
  out
}