#' Logic - Extension'
#'
#' Logical operations, extended
#'
#' @description
#' All functions take logical or logical-like (i.e., 1, 0, or NA as integer or
#'   doubles) and return logical values.
#'
#' Extensions to the base logical operations to account for `NA` values.
#'
#' [base::isTRUE()] and [base::isFALSE()] will only return single length `TRUE`
#'   or `FALSE` as it checks for valid lengths in the evaluation.  When needing
#'   to check over a vector for the presence of `TRUE` or `FALSE` and not being
#'   held back by `NA` values, `is_true` and `is_false` will always provide a
#'   `TRUE` `FALSE` when the vector is logical or return `NA` is the vector `x`
#'   is not logical.
#'
#' `%or%` is just a wrapper for [base::xor()]
#'
#' @param x,y  A vector of logical values.  If `NULL` will generate a warning.  If
#'   not a logical value, will return `NA` equal to the vector length
#' @param ... Vectors or a list of logical values
#' @param na.rm Logical, if `TRUE` will ignore `NA`
#' @export
#' @name logic_ext
#' @examples
#'
#' x <- c(TRUE, FALSE, NA)
#' y <- c(FALSE, FALSE, TRUE)
#' z <- c(TRUE, NA, TRUE)
#' isTRUE(x)
#' is_true(x)
#' isFALSE(x)
#' is_false(x)
#' TRUE %xor% TRUE
#' TRUE %xor% FALSE
#' NA %xor% FALSE
#' OR(x, y, z)
#' OR(x, y, z, na.rm = TRUE)
#' AND(x, y, z)
#' AND(x, y, z, na.rm = TRUE)
#' either(TRUE, FALSE)
#' either(FALSE, NA)
#' either(TRUE, NA)
#' is_boolean(x)
#' is_boolean(c(1L, NA_integer_, 0L))
#' is_boolean(c(1.01, 0, -1))

is_true <- function(x) {
  null_check(x)

  out <- as.logical(x)
  if (!is_boolean(x)) {
    return(out)
  }

  out[is.na(out)] <- FALSE
  out
}

#' @export
#' @rdname logic_ext
is_false <- function(x) {
  null_check(x)

  out <- as.logical(x)
  if (!is_boolean(x)) {
    return(out)
  }

  out[is.na(out)] <- TRUE
  !out
}

#' @export
#' @rdname logic_ext
`%xor%` <- function(x, y) {
  xor(x, y)
}

#' @export
#' @rdname logic_ext
OR <- function(..., na.rm = FALSE) {
  mat <- cbind(...)
  apply_logical_matrix(mat, "|", na.rm = na.rm)
}

#' @export
#' @rdname logic_ext
AND <- function(..., na.rm = FALSE) {
  mat <- cbind(...)
  apply_logical_matrix(mat, "&", na.rm = na.rm)
}

#' @export
#' @rdname logic_ext
either <- function(x, y) {
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  x | y
}

#' @export
#' @rdname logic_ext
is_boolean <- function(x) {
  if (is.logical(x)) {
    TRUE
  } else if (is.numeric(x)) {
    all(x %in% c(1, 0) | is.na(x), na.rm = TRUE)
  } else {
    FALSE
  }
}

none <- function(..., na.rm = FALSE) {
  !any(..., na.rm = na.rm)
}


# FUNS --------------------------------------------------------------------

null_check <- function(x) {
  if (is.null(x) || is_length0(x)) {
    stop("Cannot accept `NULL` or 0 length values",
         call. = FALSE)
  }
}

apply_logical_matrix <- function(mat, FUN, na.rm) {
  if (!is.matrix(mat)) {
    stop("`mat` must be a matrix", call. = FALSE)
  }

  if (!is_boolean(mat)) {
    stop("`mat` must be boolean", call. = FALSE)
  }

  na_val <-
    if (na.rm) {
      switch(FUN, `|` = FALSE, `&` = TRUE)
    } else {
      NA
    }

  use_fun <- match.fun(FUN)

  apply(
    mat,
    1,
    function(x) {
      if (na.rm) {
        x <- remove_na(x)
      }

      len <- length(x)

      if (len == 0L) {
        na_val
      } else if (len == 1L) {
        x
      } else {
        Reduce(use_fun, x)
      }
    }
  )
}
