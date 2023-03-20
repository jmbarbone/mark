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
#'
#' @seealso [match_param()]
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
#' @export
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
    csx <- as.character(substitute(x))
    stop(cond_match_arg(csx, x, table))
  }

  out
}

#' Match params
#'
#' Param matching for an argument
#'
#' @description Much like [base::match.arg()] with a few key differences:
#' * Will not perform partial matching
#' * Will not return error messages with ugly quotation marks
#'
#' @param param The parameter
#' @param choices The available choices; named lists will return the name (a
#'   character) for when matched to the value within the list element
#' @param null If `TRUE` allows `NULL` to be passed a `param`
#' @return A single value from `param` matched on `choices`
#'
#' @seealso [match_arg()]
#' @examples
#' fruits <- function(x = c("apple", "banana", "orange")) {
#'   match_param(x)
#' }
#'
#' fruits()         # apple
#' try(fruits("b")) # must be exact fruits("banana")
#'
#' # can have multiple responses
#' how_much <- function(x = list(too_few = 0:2, ok = 3:5, too_many = 6:10)) {
#'   match_param(x)
#' }
#'
#' how_much(1)
#' how_much(3)
#' how_much(9)
#' @export
match_param <- function(param, choices, null = TRUE) {
  if (is.null(param)) {
    if (null) {
      return(NULL)
    }

    stop(cond_match_param_null())
  }

  param_c <- charexpr(substitute(param))

  if (missing(choices)) {
    parent <- sys.parent()
    forms <- formals(sys.function(parent))
    choices <- eval(forms[[param_c]], envir = parent)
  }

  choices <- unlist0(choices)

  values <- names(choices) %||% choices
  param <- unlist0(param)
  res <- values[match(param[1], choices, nomatch = 0L)[[1]]]
  ocall <- outer_call()

  if (no_length(res)) {

    if (is_length0(param)) {
      param <- deparse(param)
    }

    stop(cond_match_param_match(
      input = param_c,
      argument = ocall,
      param = param,
      choices = choices
    ))
  }

  res
}

# conditions --------------------------------------------------------------

cond_match_arg <- function(csx, x, table) {
  table <-  collapse(table, sep = "\', \'")
  new_condition(
    sprintf(
      "%s : '%s' did not match of of the following:\n   '%s'",
      csx, x, table
    ),
    "cond_match_arg"
  )
}

cond_match_param_null <- function() {
  new_condition(
    "match_param() requires non-NULL params",
    "cond_match_param_null"
  )
}

cond_match_param_match <- function(
    input,
    argument,
    param,
    choices
) {
  msg <- sprintf(
    paste0(
      "`match_param(%s)` failed in `%s`:\n",
      "  `%s` [%s] must be one of the following: \"%s\""
    ),
    input,
    argument,
    input,
    param,
    collapse0(choices, sep = '", "')
  )

  new_condition(msg, "match_param_match")
}
