

# vaps --------------------------------------------------------------------

#' Vaps!
#'
#' Wrappers for vapply
#'
#' @details
#' These are simply wrappers for [base::vapply()] to shorten lines.
#'
#' Each function is designed to use specific vector types:
#'
#' \describe{
#'   \item{vap_int}{integer}
#'   \item{vap_dbl}{double}
#'   \item{vap_chr}{character}
#'   \item{vap_lgl}{logical}
#'   \item{vap_cplx}{complex}
#'   \item{vap_date}{Date}
#' }
#'
#' @param .x A vector of values
#' @param .f A function to apply to each element in vector `.x`
#' @param .nm Logical, if `TRUE` returns names of `.x` (Note: If `.x` does not
#'   have any names, they will be set to the values)
#' @param ... Additional arguments passed to `.f`
#' @return A vector of type matching the intended value in the function name.
#' @seealso [base::vapply()]
#' @export
#' @name vap
vap_int <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_integer_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_dbl <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_real_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_chr <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_character_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_lgl <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_cplx <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_complex_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_date <- function(.x, .f, ..., .nm = FALSE) {
  out <- vap_dbl(.x, .f, ..., .nm = .nm)
  as.Date.numeric(out, origin = "1970-01-01")
}

# Mostly sets up the names
do_vap <- function(.x, .f, .value, ..., .nm) {
  if (.nm) {
    .x <- set_names0(.x, names(.x) %||% .x)
  }

  vapply(X = .x, FUN = .f, FUN.VALUE = .value, ..., USE.NAMES = .nm)
}

# This can take multiple elements, so can be a little dangerous
capply <- function(.x, .f, ..., .nm = FALSE) {
  res <- do.call("c", lapply(X = .x, FUN = .f, ...))

  use_names <- if (.nm) {
    names(.x) %||% .x
  }

  set_names0(res, use_names)
}

# A simplier implementation of sapply?
slapply <- function(x, fun, ..., .simplify = TRUE, .names = TRUE) {
  res <- lapply(x, fun, ...)

  if (.simplify) {
    res <- simplify2array(res, higher = FALSE)
  }

  names(res) <- if (.names) {
    if (is.null(nm <- names(x))) x else nm
  } else {
    NULL
  }

  res
}

if (FALSE) {
  x <- 1:10
  y <- setNames(x, x)
  lapply(x, sqrt)
  lapply(y, sqrt)
  sapply(x, sqrt)
  sapply(y, sqrt)
  sapply(y, sqrt, USE.NAMES = FALSE)
  sapply(x, sqrt, USE.NAMES = TRUE)
  sapply(y, sqrt, USE.NAMES = TRUE)
  slapply(x, sqrt)
  slapply(y, sqrt)
  slapply(y, sqrt, .names = FALSE)

  lapply(setNames(nm = 1:10), sqrt)
}
