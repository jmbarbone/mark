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
#' @noRd
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
#' @return The variable `name` from package `package`
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

# modified from https://github.com/tidyverse/purrr/blob/5aca9df41452f272fcef792dbc6d584be8be7167/R/utils.R

use_color <- function() {
  rn("crayon") && crayon::has_color()
}

crayon_blue  <- function(x) if (use_color()) crayon::blue(x)  else x
crayon_green <- function(x) if (use_color()) crayon::green(x) else x
crayon_cyan  <- function(x) if (use_color()) crayon::cyan(x)  else x

#' Parse and evaluate text
#'
#' A wrapper for eval(parse(text = .))
#'
#' @param x A character string to parse
#' @param envir The environment in which to evaluate the code
#' @return The evaluation of `x` after parsing
#' @export
ept <- function(x, envir = parent.frame()) {
  eval(parse(text = x), envir = envir)
}


# Removes object's attributes before printing
print_no_attr <- function(x, ...) {
  print(remove_attributes(x))
}

#' That
#'
#' Grammatical correctness
#'
#' @details
#' See `fortunes::fortune(175)`.
#'
#' @inheritParams base::which
#' @return see [base::which()]
#'
#' @export
#' @seealso [base::which()]
that <- function(x, arr.ind = FALSE, useNames = TRUE) {
  # TODO consider that() as #seq_along(x)[x]?
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
#' @noRd
is_length0 <- function(x) {
  !is.null(x) && no_length(x)
}

#' @rdname length_check
#' @noRd
no_length <- function(x) {
  length(x) == 0L
}

#' @rdname length_check
#' @noRd
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

mark_temp <- function(ext = "") {
  if (!grepl("^[.]", ext) && !identical(ext, "") && !is.na(ext)) {
    ext <- paste0(".", ext)
  }

  # Retrieves the outer most function this was called in and save the raw
  # components to be converted when needed
  sn <- sys.nframe()
  oc <- outer_call(sn - 2L)
  oc <- substr(oc, 1, 40)
  oc <- collapse0(iconv(oc, toRaw = TRUE)[[1]])
  oc <- paste0(oc, "__")
  norm_path(tempfile(oc, fileext = ext))
}


check_is_vector <- function(x, mode = "any") {
  if (isS4(x) | inherits(x, c("data.frame", "matrix", "array")) | !is.vector(remove_attributes(x), mode)) {
    stop(deparse(substitute(x)), " must be a vector of mode ", mode, call. = FALSE)
  }
}

add_attributes <- function(x, ...) {
  attributes(x) <- c(attributes(x), list(...))
  x
}

remove_attributes <- function(x, attr = NULL) {
  if (is.null(attr)) {
    attributes(x) <- NULL
  } else {
    a <- attributes(x)
    attributes(x) <- a[names(a) %wo% attr]
  }
  x
}

add_class <- function(x, cl, pos = 1L) {
  class(x) <- append0(class(x), cl, pos = pos)
  x
}

remove_class <- function(x, cl = NULL) {
  if (is.null(cl)) {
    class(x) <- NULL
  } else {
    class(x) <- class(x) %wo% cl
  }
  x
}

append0 <- function(x, values, pos = NULL) {
  if (is.null(pos)) {
    return(c(x, values))
  }

  if (pos == 1L) {
    return(c(values, x))
  }

  n <- length(x)
  pos <- min(pos, n)
  c(x[1L:(pos - 1L)], values, x[pos:n])
}

check_interactive <- function() {
  if (!isFALSE(getOption("mark.check_interactive"))) {
    return(interactive())
  }

  TRUE
}


try_formats <- function(date = FALSE) {
  x <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H %M %S",
    "%Y %m %d %H %M %S",
    "%Y-%m-%d %H%M%S",
    "%Y %m %d %H%M%S",
    "%Y%m%d %H %M %S",
    "%Y%m%d %H%M%S",
    NULL
  )

  c(x, paste(x, "%Z"), if (date) c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"))
}
