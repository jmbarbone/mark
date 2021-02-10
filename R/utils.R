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
  NextMethod("print", x)
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


is_length0 <- function(x) {
  !is.null(x) && length(x) == 0L
}


#' Limit
#'
#' Limit a numeric vector by lower and upper bounds
#'
#' @param x A numeric vector
#' @param lower A lower limit (as `x < lower`)
#' @param upper An upper limit (as `x > higher`)
#'
#' @export
limit <- function(x, lower = min(x), upper = max(x)) {
  if (lower > upper) {
    stop("`lower` cannot be more than `upper`", call. = FALSE)
  }

  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}


#' Median (Q 50)
#'
#' Median as the 50th quantile with an option to select quantile algorithm
#'
#' @details
#' `q50` is an alias for `median2`
#'
#' @inheritParams stats::quantile
#' @examples
#' set.seed(42)
#' x <- rnorm(100)
#' median(x)            # 0.08979677
#' median2(x, type = 7) # 0.08979677 - default type is 7
#' median2(x, type = 3) # 0.08976065
#'
#' @export
#' @seealso [stats::quantile()]

median2 <- function(x, type = 7, na.rm = FALSE) {
  stats::quantile(x, probs = .5, type = type, na.rm = na.rm, names = FALSE)
}

#' @export
#' @rdname median2
q50 <- median2


#' Range 2
#'
#' Employs `min()` and `max()`.  However, [base::range()], there is no argument
#'   for removing `Inf` values.
#'
#' @param x A numeric (or character) vector (see Note in [base::min])
#' @param na.rm Logical, if `TRUE` removes missing values
#'
#' @examples
#' x <- c(1:1e5)
#' system.time(rep(range(x),  100))
#' system.time(rep(range2(x), 100))
#' x[sample(x, 1e4)] <- NA
#'
#' system.time(rep(range(x), 100))
#' system.time(rep(range2(x), 100))
#' system.time(rep(range(x, na.rm = TRUE), 100))
#' system.time(rep(range2(x, na.rm = TRUE), 100))
#'
#' @export
range2 <- function(x, na.rm = FALSE) {
  c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

is_unique <- function(x) {
  all(!duplicated(x))
}

as_character <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[x])
  }

  as.character(x)
}

pseudo_id <- function(x) {
  match(x, unique(x))
}
