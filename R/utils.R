
which0 <- function(x) {
  which(x) %len% 0L
}
# isTRUE, isFALSE, ...
isNA <- function(x) { # nolint: object_name_linter.
  is.logical(x) && length(x) == 1L && is.na(x)
}

# modified from https://github.com/tidyverse/purrr/blob/5aca9df41452f272fcef792dbc6d584be8be7167/R/utils.R # nolint: line_length_linter.
use_color <- function() {
  rn("crayon") && crayon::has_color()
}

# nolint start: brace_linter.
crayon_blue  <- function(x) { if (use_color()) crayon::blue(x)  else x }
crayon_green <- function(x) { if (use_color()) crayon::green(x) else x }
crayon_cyan  <- function(x) { if (use_color()) crayon::cyan(x)  else x }
# nolint end: brace_linter.

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
  print(remove_attributes(x)) # nocovr
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
that <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint: object_name_linter, line_length_linter.
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

is_atomic0 <- function(x) {
  is.atomic(x) && !is.null(x)
}

# nolint start: brace_linter.
cat0 <- function(...) { cat(..., sep = "") }
catln <- function(...) { cat(..., sep = "\n") }
charexpr <- function(x) { as.character(as.expression(x)) }
# nolint end: brace_linter.

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
  if (
    isS4(x) ||
    inherits(x, c("data.frame", "matrix", "array")) ||
    !is.vector(remove_attributes(x), mode)
  ) {
    x <- deparse1(substitute(x))
    stop(cond_check_is_vector_mode(x, mode))
  }

  invisible()
}

cond_check_is_vector_mode <- function(x, mode) {
  new_condition(
    paste(x, "must be a vector of mode", mode),
    "check_is_vector_mode"
  )
}


add_attributes <- function(x, ...) {
  attributes(x) <- c(attributes(x), rlang::list2(...))
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

check_interactive <- function() {
  op <- getOption("mark.check_interactive", TRUE)

  if (isTRUE(op)) {
    return(interactive())
  }

  if (isFALSE(op)) {
    return(TRUE)
  }

  if (isNA(op)) {
    return(FALSE)
  }

  stop(cond_check_interactive())
}

cond_check_interactive <- function() {
  new_condition(
    "mark.check_interactive must be TRUE, FALSE, or NA",
    "check_interactive"
  )
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

  c(x, if (date) c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"))
}

has_char <- function(x) {
  if (!is.character(x)) {
    return(rep.int(FALSE, length(x)))
  }

  !is.na(x) & nzchar(x, keepNA = TRUE)
}

dupe_check <- function(x, n = getOption("mark.dupe.n", 5)) {
  n <- as.integer(n)

  dupes <- which(duplicated(x))
  n_dupes <- length(dupes)
  dupes <- utils::head(dupes, n)

  if (n_dupes) {
    stop(cond_dupe_check(x, dupes, n_dupes, n))
  }

  invisible(NULL)
}

cond_dupe_check <- function(x, dupes, n_dupes, n) {
  msg <- paste0(
    "Duplicate values found in ", n_dupes, " location(s) :\n",
    if (n_dupes > n) sprintf("(first %i)\n", n),
    paste0("  > ", sprintf("[%s] %s", format(dupes), format(x[dupes])), "\n"),
    if (n_dupes > n) "... and ", n_dupes - n, " more"
  )

  new_condition(msg, "dupe_check")
}
