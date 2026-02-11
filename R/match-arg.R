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
#' @seealso [mark::match_param()]
#' @examples
#' x <- c("apple", "banana", "orange")
#' suppressWarnings(match_arg("b", x), "deprecatedWarning")
#'
#' # Produces error
#' suppressWarnings(try(match_arg("pear", x)), "deprecatedWarning")
#'
#' foo <- function(x, op = c(1, 2, 3)) {
#'   op <- suppressWarnings(match_arg(op), "deprecatedWarning")
#'   x / op
#' }
#'
#' foo(10, 3)
#'
#' # Error
#' try(foo(1, 0))
#' @export
match_arg <- function(x, table) {
  .Deprecated("match_param()")
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
    stop(match_arg_no_match(csx, x, table))
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
#'   character) for when matched to the value within the list element.  A list
#'   of formula objects (preferred) retains the LHS of the formula as the return
#'   value when matched to the RHS of the formula.
#' @param null If `TRUE` allows `NULL` to be passed a `param`
#' @param partial If `TRUE` allows partial matching via [base::pmatch()]
#' @param multiple If `TRUE` allows multiple values to be returned
#' @param simplify If `TRUE` will simplify the output to a single value
#' @return A single value from `param` matched on `choices`
#'
#' @section Conditions:
#' `r cnd::cnd_section("match_param")`
#'
#' @seealso [mark::match_arg()]
#' @examples
#' fruits <- function(x = c("apple", "banana", "orange")) {
#'   match_param(x)
#' }
#'
#' fruits()         # apple
#' try(fruits("b")) # must be exact fruits("banana")
#'
#' pfruits <- function(x = c("apple", "apricot", "banana")) {
#'   match_param(x, partial = TRUE)
#' }
#' pfruits()          # apple
#' try(pfruits("ap")) # matchParamMatchError
#' pfruits("app")     # apple
#'
#' afruits <- function(x = c("apple", "banana", "orange")) {
#'   match_param(x, multiple = TRUE)
#' }
#'
#' afruits() # apple, banana, orange
#'
#' # can have multiple responses
#' how_much <- function(x = list(too_few = 0:2, ok = 3:5, too_many = 6:10)) {
#'   match_param(x)
#' }
#'
#' how_much(1)
#' how_much(3)
#' how_much(9)
#'
#' # use a list of formulas instead
#' ls <- list(1L ~ 0:1, 2L, 3L ~ 3:5)
#' sapply(0:5, match_param, choices = ls)
#' @export
match_param <- function(
  param,
  choices,
  null = TRUE,
  partial = getOption("mark.match_param.partial", FALSE),
  multiple = FALSE,
  simplify = TRUE
) {
  param_c <- charexpr(substitute(param))
  force(param)

  if (is.null(param)) {
    if (null) {
      return(NULL)
    }

    stop(input_error("match_param() requires non-NULL params"))
  }

  missing_choices <- missing(choices)
  if (missing_choices) {
    parent <- sys.parent()
    forms <- formals(sys.function(parent))
    choices <- eval(forms[[param_c]], envir = parent)
  }

  mparam <- cleanup_param_list(param)
  mchoices <- cleanup_param_list(choices)

  if (anyDuplicated(unlist(mchoices$choices))) {
    # TODO implement cond_match_param_dupes()
    to_choices <- function(x) {
      if (all(names(x) == as.character(x))) {
        dupe <- duplicated(x)
        x[dupe] <- paste0(x[dupe], "*")
        return(toString(x))
      }

      collapse(
        mapply(
          function(x, nm, d) {
            sprintf(
              "%s = %s",
              nm,
              toString(paste0(x, ifelse(d, "*", "")))
            )
          },
          x = x,
          nm = names(x),
          d = split(
            duplicated(unlist(x)),
            rep(seq_along(x), lengths(x))
          ),
          USE.NAMES = FALSE
        ),
        sep = "\n  "
      )
    }

    # TODO use duplicate_error()
    stop(input_error(paste0(
      "duplicate values found in `choices`:\n  ",
      to_choices(choices)
    )))
  }

  fun <- if (partial) pmatch else match
  m <- fun(mparam$choices, mchoices$choices, nomatch = NA_integer_)

  if (!multiple && length(m)) {
    m <- m[1L]
  }

  ocall <- outer_call()
  if (no_length(param) || identical(m, NA_integer_)) {
    if (is_length0(param)) {
      param <- deparse(param)
    }

    stop(match_param_error(
      input = param_c,
      argument = ocall,
      param = param,
      choices = choices
    ))
  }

  res <- lapply(m, function(i) mchoices$values[[i]])

  if (simplify) {
    res <- unlist0(res)
  }

  res
}

cleanup_param_list <- function(x) {
  x <- as.list(x)
  env <- parent.frame()

  eval_expr <- function(x, i) {
    eval(as.expression(x[[i]]), env)
  }

  out <- list(values = NULL, choices = NULL)
  nms <- names(x)
  for (i in seq_along(x)) {
    if (inherits(x[[i]], "formula")) {
      out$values[[i]] <- eval_expr(x[[i]], 2L)
      out$choices[[i]] <- eval_expr(x[[i]], 3L)
    } else {
      out$values[[i]] <- if (isTRUE(nzchar(nms[i]))) nms[i] else x[[i]]
      out$choices[[i]] <- x[[i]]
    }
  }

  out$values <- rep(out$values, lengths(out$choices))
  out$choices <- unlist(lapply(out$choices, as.list), recursive = FALSE)
  out
}

# conditions --------------------------------------------------------------

# TODO remove
match_arg_no_match := condition(
  message = function(csx, x, table) {
    sprintf(
      "%s : '%s' did not match of of the following:\n   '%s'",
      csx,
      x,
      table
    )
  },
  type = "error"
)

match_param_error := condition(
  message = function(input, argument, param, choices) {
    to_value <- function(x) {
      if (all(names(x) == as.character(x))) {
        return(toString(x))
      }

      nms <- names(x)
      if (is.null(nms)) {
        return(toString(x))
      }

      ok <- nzchar(nms)
      nms[ok] <- paste(nms[ok], "= ")
      toString(paste0(nms, x))
    }

    to_options <- function(x) {
      if (all(names(x) == as.character(x))) {
        return(toString(x))
      }

      collapse(
        mapply(
          function(x, nm) {
            if (nzchar(nm)) {
              sprintf("%s = %s", nm, toString(x))
            } else {
              toString(x)
            }
          },
          x = x,
          nm = names(x),
          USE.NAMES = FALSE
        ),
        sep = " | "
      )
    }

    sprintf(
      paste0(
        "`match_param(%s)` failed in `%s`:\n",
        "  param    %s\n",
        "  choices  %s"
      ),
      input,
      argument,
      to_value(param),
      to_options(choices)
    )
  },
  type = "error",
  exports = "match_param"
)
