#' Recode by
#'
#' A simple implementation of recoding
#'
#' @details
#' This can be comparable to [dplyr::recode()] expect that the values are
#'   arranged as `new = old` rather than `old = new` and allows for a separate
#'   vector to be passed for `new`.
#'
#' `recode_only()` will only recode the values matches in `by`/`val`.
#'   The `mode` is automatically set according to `mode(x)`.  This functions
#'   more like [base::replace()] but with extra features
#'
#' @param x A vector to recode
#' @param by A names vector (`new = old`); any non-matching values are set to
#'   the appropriate `NA`
#' @param vals An optional vector of values to use in lieu of a names in the
#'   vector; this takes priority over `names(by)`.  This can be the same length
#'   as `by` or a single value.
#' @param mode passed to `as.vector()`
#' @return A vector of values from `x`
#'
#' @examples
#' recode_by(1:3, c(a = 1, b = 2))
#' recode_by(letters[1:3], c(`1` = "a", `2` = "b"))                   # will not guess mode
#' recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer") # make as integer
#' recode_by(letters[1:3], c("a", "b"), vals = 1:2)                   # or pass to vals
#'
#' recode_only(letters[1:3], c("zzz" = "a"))
#' recode_only(letters[1:3], c(`1` = "a")) # returns as "1"
#' recode_only(1:3, c("a" = 1))            # coerced to NA
#'
#' # Pass list for multiples
#' recode_only(letters[1:10], list(abc = c("a", "b", "c"), ef = c("e", "f")))
#'
#' @seealso [dplyr::recode()]
#' @export

recode_by <- function(x, by, vals = NULL, mode = "any") {

  if (is.factor(x)) {
    levels(x) <- recode_by(levels(x), by = by, vals = vals, mode = mode)
    return(x)
  }

  if (is.list(by)) {
    by <- unlist0(by)
  }

  vals <- vals %||% names(by)

  if (is.null(vals)) {
    stop("values to recode by were not properly set", call. = FALSE)
  }

  if (length(vals) == 1) {
    vals <- rep.int(vals, length(by))
  }

  as.vector(vals[match(x, by)], mode = mode)
}

#' @export
#' @rdname recode_by
recode_only <- function(x, by, vals = NULL) {

  if (is.factor(x)) {
    levels(x) <- recode_only(levels(x), by = by, vals = vals)
    return(x)
  }

  if (is.list(by)) {
    by <- unlist0(by)
  }

  vals <- vals %||% names(by)

  if (is.null(vals)) {
    stop("values to recode by were not properly set", call. = FALSE)
  }

  if (is.list(vals)) {
    vals <- unlist0
  }

  vals <- as.vector(vals, if (typeof(x) == "integer") "integer" else mode(x))

  if (length(vals) == 1L) {
    x[x %in% by] <- vals
    return(x)
  }

  m <- match(x, by, nomatch = 0L)
  mode <- mode(x)
  x[m > 0L] <- vals[m]
  clean_na_coercion(as.vector(x, mode = mode))
}

# helpers -----------------------------------------------------------------

clean_na_coercion <- function(expr) {
  res <- warn <- NULL
  local({
    withCallingHandlers(
      res <<- expr,
      warning = function(e) {
        if (grepl("NAs introduced by coercion$", e$message)) {
          warn <<- e$message
          invokeRestart("muffleWarning")
        }
      }
    )
  })

  if (!is.null(warn)) {
    warning(warningCondition(warn))
  }

  res
}
