# TODO these will be deprecated by fuj::vap

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
#'   \item{[mark::vap_int()]}{integer}
#'   \item{[mark::vap_dbl()]}{double}
#'   \item{[mark::vap_chr]()}{character}
#'   \item{[mark::vap_lgl]()}{logical}
#'   \item{[mark::vap_cplx]()}{complex}
#'   \item{[mark::vap_date]()}{Date}
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
  .x <- set_names(.x, names(.x) %||% .x)
  vapply(X = .x, FUN = .f, FUN.VALUE = .value, ..., USE.NAMES = .nm)
}

capply <- function(x, f) {
  do.call(base::c, lapply(x, f))
}
