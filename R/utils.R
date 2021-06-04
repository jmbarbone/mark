#' @importFrom magrittr %>%
magrittr::`%>%`

# Like rlang::`%||%` but uses base is.null -- same thing

#' Default value for NULL
#'
#' Replace if `NULL`
#'
#' @details
#' A mostly copy of `rlang`'s `%||%` except does not use [rlang::is_null()],
#'   which, currently, calls the same primitive `is.null` function as
#'   [base::is.null()].
#' This is not to be exported due to conflicts with `purrr`
#'
#' @param x,y If `x` is `NULL` returns `y`; otherwise `x`
#'
#' @name null_default
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Colons
#'
#' Get an object from a package
#'
#' @details
#' This is a work around to calling `:::`.
#'
#' @section WARNING:
#' To reiterate from other documentation: it is not advised to use `:::` in
#'   your code as it will retrieve non-exported objects that may be more
#'   likely to change in their functionality that exported objects.
#'
#' @param package Name of the package
#' @param name Name to retrieve
#'
#' @export
`%colons%` <- function(package, name) {
  tryCatch(
    get(name, envir = asNamespace(package)),
    error = function(e) {
      stop(sprintf("`%s` not found in package `%s`",
                   name, package),
           call. = FALSE)
    }
  )
}

#' Parse and evaluate text
#'
#' A wrapper for eval(parse(text = .))
#'
#' @param x A character string to parse
#' @param envir The environment in which to evaluate the code
#' @export
ept <- function(x, envir = parent.frame()) {
  eval(parse(text = x), envir = envir)
}


# Removes object's attributes before printing
print_no_attr <- function(x, ...) {
  attributes(x) <- NULL
  print(x)
}

#' That
#'
#' Grammatical correctness
#'
#' @details
#' See `fortunes::fortune(175)`.
#'
#' @inheritParams base::which
#'
#' @export
#' @seealso [base::which()]
that <- function(x, arr.ind = FALSE, useNames = TRUE) {
  which(x, arr.ind = arr.ind, useNames = useNames)
}

#' Length checkers
#'
#' Checks lengths
#'
#' @description
#' Several length checks exist:
#'
#' * `is_length0`: Not `NULL` but is length `0`
#' * `no_length`: Length of `0`
#' * `has_length`: Length is not `0`
#'
#' _NB_: `length(NULL)` is `0`
#'
#' @param x A vector
#' @name length_check
is_length0 <- function(x) {
  !is.null(x) && no_length(x)
}

#' @rdname length_check
no_length <- function(x) {
  length(x) == 0L
}

#' @rdname length_check
has_length <- function(x) {
  !no_length(x)
}

is_unique <- function(x) {
  anyDuplicated(x) == 0L
}

as_character <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[x])
  }

  as.character(x)
}

is_atomic0 <- function(x) {
  is.atomic(x) && !is.null(x)
}

which_unwrap <- function(w, n = max(w)) {
  n <- max(n, max(w)) # protective
  x <- logical(n)
  x[w] <- TRUE
  x
}

cat0 <- function(...) cat(..., sep = "")
catln <- function(...) cat(..., sep = "\n")
charexpr <- function(x) as.character(as.expression(x))
