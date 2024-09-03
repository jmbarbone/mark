#' Switch with a list of parameters
#'
#' Switch with a list of params
#'
#' @description
#' `switch_params()` is a vectorized version of `switch`
#' `switch_case()` uses a formula syntax to return the value to the right of the
#'   tilde (`~`) when `x` is `TRUE`
#' `switch_in_case()` is a special case of `switch_case()` for `match()`-ing `x`
#'   in the values on the left to return the value on the right.
#'
#' @return A named vector of values of same length `x`; or for `switch_case`,
#'   an unnamed vector of values matching the rhs of `...`
#'
#' Inspired from:
#' * https://stackoverflow.com/a/32835930/12126576
#' * https://github.com/tidyverse/dplyr/issues/5811
#'
#' @examples
#' # by single
#' switch_params(c("j", "m", "b"), j = 10, b = 2, m = 13)
#'
#'
#' # match with TRUE
#' switch_case(
#'   1:10 == 9      ~ NA_integer_,
#'   1:10 %% 3 == 0 ~ 1:10,
#'   1:10 %% 4 == 0 ~ 11:20,
#'   1:10 %% 5 == 0 ~ 21:30,
#'   1:10 %% 2 == 0 ~ 31:40,
#'   .default = -1L
#' )
#'
#' # match within a vector
#' switch_in_case(
#'   c(1, 2, 12, 4, 20, 21),
#'   1:10  ~ 1,
#'   11:20 ~ 2
#' )
#'
#' switch_in_case(
#'   c("a", "b", "d", "e", "g", "j"),
#'   letters[1:3] ~ "a",
#'   letters[5:6] ~ "e"
#' )
#'
#' use_these <- c(1, 3, 2, 5)
#' switch_in_case(
#'   1:10,
#'   use_these ~ TRUE,
#'   .default = FALSE
#' )
#'
#' ne <- new.env()
#' ne$use_these2 <- use_these
#' # error
#' try(switch_in_case(
#'   1:10,
#'   use_these2 ~ TRUE
#' ))
#' switch_in_case(
#'   1:10,
#'   use_these2 ~ TRUE,
#'   .envir = ne
#' )
#'
#' switch_in_case(
#'   seq.int(1, 60, 6),
#'   1:10          ~ "a",
#'   11:20         ~ "b",
#'   c(22, 24, 26) ~ "c",
#'   30:Inf        ~ "d"
#' )
#'
#' # Use functions
#' switch_in_case(
#'   1:6,
#'   c(1, 3, 5) ~ exp,
#'   c(2, 4) ~ log
#' )
#' @name switch-ext
NULL

#' @param x A vector of values
#' @param ... Case evaluations (named for `switch_params`)
#' @rdname switch-ext
#' @export
switch_params <- function(x, ...) {
  ls <- rlang::list2(...)
  y <- as.vector(ls, mode = mode(ls[[1L]]))
  nmls <- names(ls)
  names(y) <- nmls
  y[match(x, nmls)]
}

#' @param .default The default value if no matches are found in `...`
#'   (default: `NULL` produces an `NA` value derived from `...`)
#' @param .envir The environment in which to evaluate the LHS of `...` (default:
#'   `parent.frame()`)
#' @rdname switch-ext
#' @export
switch_in_case <- function(x, ..., .default = NULL, .envir = parent.frame()) {
  ls <- rlang::list2(...)

  # split by the tilde
  splits <- strsplit(as.character(ls), "\\s?~\\s?")

  if (is.numeric(x)) {
    # Get finite ranges -- add buffers, just in case?
    fins <- x[is.finite(x)]
    xrange <- c(
      min(fins - 1L, na.rm = TRUE),
      max(fins + 1L, na.rm = TRUE)
    )
  }

  rhs <- lapply(splits, function(i)  {
    eval(parse(text = i[2L]), envir = parent.frame())
  })

  lhs <- lapply(splits, function(i) {
    # A bit more intensive to deal with "1:Inf" and what not
    res <- try_catch_inf(eval(parse(text = i[1L]), envir = .envir))
    if (!is.call(res)) {
      return(res)
    }

    cres <- as.character(res)
    if (cres[1L] == ":") {
      if (!exists("xrange")) {
        stop(cond_switch_in_case_numeric())
      }

      cres[cres == "-Inf"] <- xrange[1L]
      cres[cres == "Inf"] <- xrange[2L]
      if (any(cres %in% c("-Inf", "inf"))) {
        stop(cond_switch_in_case_ambiguous())
      }
    }

    tryCatch(
      do.call(cres[1L], as.list(cres[-1L])),
      error = function(e) {
        stop(cond_switch_in_case_evaluate(e$message))
      }
    )
  })

  # set the default NA value
  res0 <- .default %||% NA

  do_switches <- function(xi) {
    resi <- res0

    for (i in seq_along(ls)) {
      w <- which(xi %in% lhs[[i]])
      if (length(w)) {
        # only change if found
        resi <- rhs[[i]]

        if (is.function(resi)) {
          resi <- match.fun(resi)
          resi <- resi(xi[w])
        }

        break
      }
    }
    resi
  }

  res <- lapply(x, do_switches)
  stopifnot(lengths(res) == 1L)
  res <- unlist(res)
  mode(res) <- mode(res[[1]])
  class(res) <- class(res[[1]])
  names(res) <- x
  res
}

#' @rdname switch-ext
#' @export
switch_case <- function(..., .default = NULL, .envir = parent.frame()) {
  ls <- rlang::list2(...)

  # split by the tilde
  splits <- strsplit(as.character(ls), "\\s?~\\s?")
  lhs <- lapply(splits, function(i) eval(parse(text = i[1L]), envir = .envir))
  rhs <- lapply(splits, function(i) eval(parse(text = i[2L]), envir = .envir))

  lhs <- switch_length_check(lhs)
  rhs <- switch_length_check(rhs)
  switch_lengths_check(lhs, rhs)

  # set the default NA value
  res0 <- .default %||% rhs[[1L]][0L][NA]

  # create as matrices
  lmat <- Reduce(cbind, lhs, right = FALSE)
  rmat <- Reduce(cbind, rhs, right = TRUE)
  w <- apply(lmat, 1L, function(x) which(x)[1])
  inds <- cbind(seq_along(w), w)
  out <-
    if (nrow(rmat) == nrow(inds)) {
      rmat[inds]
    } else {
      rmat[inds[, 2, drop = FALSE]]
  }
  out[rowSums(lmat) == 0L] <- res0
  as.vector(out, mode(res0))
}

# FUNS --------------------------------------------------------------------

try_catch_inf <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (grepl("result would be too long a vector", e$message)) {
        return(e$call)
      } else {
        stop(errorCondition(e$message, class = e$class, call = e$call))
      }
    }
  )
}

switch_length_check <- function(ls) {
  lens <- lengths(ls)
  u <- sort(unique(lens))

  switch(
    length(u),
    return(ls),
    {
      if (u[1L] == 0L) {
        stop(cond_switch_length_check_0())
      }

      if (u[1L] == 1L) {
        ind <- which(lens == 1L)

        for (i in ind) {
          ls[[i]] <- rep.int(ls[[i]], u[2L])
        }

        return(ls)
      }
      stop(cond_switch_length_check_2())
    },
    stop(cond_switch_length_check_3())
  )
  stop(cond_switch_length_check_bad())
}

switch_lengths_check <- function(lhs, rhs) {
  llens <- unique(lengths(lhs))
  rlens <- unique(lengths(rhs))

  if (llens == 1L || rlens == 1L) {
    return(invisible(NULL))
  }

  if (!identical(llens, rlens)) {
    stop(cond_switch_lengths_check())
  }

  return(invisible(NULL))
}

# conditions --------------------------------------------------------------

cond_switch_in_case_numeric <- function() {
  new_condition(
    "x did not appear to be numeric, cannot continue evaluating lhs",
    "switch_in_case_numeric"
  )
}

cond_switch_in_case_ambiguous <- function() {
  new_condition(
    "Ambiguous infinity, cannot calculate",
    "switch_in_case_ambiguous"
  )
}

cond_switch_in_case_evaluate <- function(x) {
  new_condition(
    paste0("Could not evaluate lhs\n", x),
    "switch_in_case_evaluate"
  )
}

cond_switch_lengths_check <- function() {
  new_condition("statements have different lengths", "switch_lengths_check")
}

cond_switch_length_check_0 <- function() {
  new_condition("Cannot have 0 length rhs", "switch_length_check_0")
}

cond_switch_length_check_2 <- function() {
  new_condition(
    "2 lengths found, one of which was not 1",
    "switch_length_check_1"
  )
}

cond_switch_length_check_3 <- function() {
  new_condition("3 or more lengths found, stopping", "switch_length_check_3")
}

cond_switch_length_check_bad <- function() {
  new_condition("Something really went wrong", "switch_length_check_bad")
}

# terminal line
