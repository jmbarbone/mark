
# fact --------------------------------------------------------------------

#' Factor
#'
#' Quickly create a factor
#'
#' @details `fact()` can be about 5 times quicker than `factor()` or
#'   `as.factor()` as it doesn't bother sorting the levels for non-numeric data
#'   or have other checks or features.  It simply converts a vector to a factor
#'   with all unique values as levels with `NA`s included.
#'
#'   `fact.factor()` will perform several checks on a factor to include `NA`
#'   levels and to check if the levels should be reordered to conform with the
#'   other methods.  The `fact.fact()` method simple returns `x`.
#'
#'   @section level ordered:
#'   The order of the levels may be adjusted to these rules depending on the class of `x`:
#'   \describe{
#'     \item{character}{The order of appearance}
#'     \item{numeric/integer/Date/POSIXt}{By the numeric order}
#'     \item{logical}{As `TRUE`, `FALSE`, then `NA` if present}
#'     \item{factor}{Numeric if levels can be safely converted, otherwise as they are}
#'   }
#'
#' @param x A vector of values
#' @return A vector of equal length of `x` with class `fact` and `factor`.  If
#'   `x` was `ordered`, that class is added in between.
#' @export
fact <- function(x) {
  UseMethod("fact", x)
}

#' @rdname fact
#' @export
fact.default <- function(x) {
  stop("No fact method for class(es) ", collapse0(class(x), sep = ", "),
       call. = FALSE)
}

#' @rdname fact
#' @export
fact.character <- function(x) {
  out <- pseudo_id(x)
  make_fact(out, .uniques(out))
}

#' @rdname fact
#' @export
fact.numeric <- function(x) {
  u <- sort.int(unique(x), method = "radix", na.last = TRUE)
  make_fact(match(x, u), u)
}

#' @rdname fact
#' @export
fact.integer <- fact.numeric

#' @rdname fact
#' @export
fact.Date <- fact.numeric

#' @rdname fact
#' @export
fact.POSIXt <- fact.numeric

#' @rdname fact
#' @export
fact.logical <- function(x) {
  out <- as.integer(x)
  w <- which(!x)
  out[w] <- out[w] + 2L
  nas <- is.na(x)
  out[nas] <- 3L
  make_fact(out, levels = c("TRUE", "FALSE", if (any(nas)) NA))
}

#' @rdname fact
#' @export
fact.factor <- function(x) {
  old_levels <- levels(x)
  new_levels <- fact_coerce_levels(old_levels)

  if (is.logical(new_levels)) {
    m <- match(new_levels[x], c(TRUE, FALSE, NA))
    res <- make_fact(m, c("TRUE", "FALSE", if (anyNA(new_levels)) NA_character_))
    return(res)
  }

  if (inherits(new_levels, c("numeric", "Date", "POSIXt"))) {
    ord_levels <- sort(new_levels, na.last = TRUE)
    o <- match(old_levels, as.character(ord_levels))

    levels <- as.character(c(ord_levels, if (anyNA(x) && !anyNA(ord_levels)) NA))

    if (identical(o, seq_along(o))) {
      res <- make_fact(x, levels, is.ordered(x))
      return(res)
    }

    m <- match(order(old_levels), o)[x]
    res <- make_fact(m, levels, is.ordered(x))
    return(res)
  }


  if (anyNA(x) || anyNA(old_levels)) {
    new_levels <- if (!anyNA(new_levels)) {
      c(new_levels, NA)
    } else {
      na_last(new_levels)
    }
  }

  m <- match(old_levels, as.character(new_levels))[x]
  make_fact(m, new_levels, is.ordered(x))
}

#' @rdname fact
#' @export
fact.fact <- function(x) {
  x
}

#' @rdname fact
#' @export
fact.pseudo_id <- function(x) {
  u <- .uniques(x)

  # check if numeric and already ordered
  if (is.numeric(u)) {
    o <- order(u)
    if (!identical(o, u)) {
      x <- match(u[o], u)[x]
      u <- u[o]
    }
  }

  make_fact(x, levels = u)
}

#' @rdname fact
#' @export
fact.haven_labelled <- function(x) {
  require_namespace("haven")
  labels <- sort(attr(x, "labels", exact = TRUE))
  uc <- unclass(x)
  u <- unique(uc)
  m <- match(u, labels)
  nas <- is.na(m)

  if (any(nas)) {
    # Done to match haven::as_factor.haven_labelled()
    labels <- labels[m]
    labels[nas] <- u[nas]
    names(labels)[nas] <- u[nas]
    mx <- match(x, labels)
  } else {
    mx <- as.integer(x)
  }

  struct(
    mx,
    class = c("fact", "factor"),
    levels = names(labels),
    label = attr(x, "label", exact = TRUE)
  )
}

#' @export
print.fact <- function(x, ...) {
  out <- x
  class(x) <- setdiff(class(x), "fact")
  print(x)
  invisible(out)
}


# as_ordered --------------------------------------------------------------

#' Ordered
#'
#' As ordered
#'
#' @details
#' Simple implementation of `ordered`.  If `x` is `ordered` it is simply
#'   returned.  If `x` is a `factor` the `ordered` class is added.  Otherwise,
#'   `x` is made into a `factor` with [mark::fact()] and then the `ordered`
#'   class is added.
#'   Unlike just `fact`, `ordered` will replace the `NA` levels with `NA_integer_` to work appropriately with other functions.
#'
#' @inheritParams fact
#' @export
#' @returns An `ordered` vector
#' @examples
#' x <- c("a", NA, "b")
#' x <- fact(x)
#' str(x) # NA is 3L
#'
#' y <- x
#' class(y) <- c("ordered", class(y))
#' max(y)
#' max(y, na.rm = TRUE) # returns NA -- bad
#'
#' # as_ordered() removes the NA level
#' x <- as_ordered(x)
#' str(x)
#' max(x, na.rm = TRUE) # returns b -- correct

as_ordered <- function(x) {
  UseMethod("as_ordered", x)
}

#' @rdname as_ordered
#' @export
as_ordered.default <- function(x) {
  as_ordered(fact_remove_na(x))
}

#' @rdname as_ordered
#' @export
as_ordered.factor <- function(x) {
  add_class(fact_remove_na(x), "ordered", 2L)
}

#' @rdname as_ordered
#' @export
as_ordered.ordered <- function(x) {
  fact_remove_na(x)
}


# helpers -----------------------------------------------------------------

make_fact <- function(x, levels, ordered = FALSE) {
  struct(
    x,
    class = c("fact", if (ordered) "ordered", "factor"),
    levels = as.character(levels)
  )
}

fact_remove_na <- function(x) {
  x <- fact(x)
  out <- unclass(x)
  lvl <- levels(out)
  w <- which(is.na(lvl))

  if (!length(w)) {
    return(x)
  }

  out[out == w] <- NA_integer_
  make_fact(out, levels = lvl[-w])
}

try_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  nas <- is.na(x)

  if (all(nas)) {
    return(x)
  }

  nums <- wuffle(as.numeric(x[!nas]))

  if (anyNA(nums)) {
    return(x)
  }

  nums[nas] <- NA
  nums
}

fact_coerce_levels <- function(x) {
  nas <- is.na(x)

  if (all(nas) || !anyNA(match(x[!nas], c("TRUE", "FALSE")))) {
    return(as.logical(x))
  }

  numbers <- wuffle(as.numeric(x[!nas]))
  dates <- wuffle(as.Date(x[!nas], optional = TRUE))
  posix <- wuffle(as.POSIXlt(x[!nas], tz = getOption("mark.default_tz", "UTC"), optional = TRUE))

  n <- length(x)

  if (!anyNA(posix)) {
    x <- rep(NA_POSIXlt_, n)
    x[!nas] <- posix
  } else if (!anyNA(dates)) {
    x <- rep(NA_Date_, n)
    x[!nas] <- dates
  } else if (!anyNA(numbers)) {
    x <- rep(NA_real_, n)
    x[!nas] <- numbers
  }

  x
}

# x <- factor(c("a", NA), levels = c("a", "b"))
# res <- fact(x)
# fact_levels(res) <- c("1", "a", "b", "c")
# res

`fact_levels<-` <- function(x, value) {
  x <- fact(x)
  levels <- levels(x)
  value <- c(value, if (!anyNA(value) & (anyNA(x) | anyNA(levels))) NA)
  make_fact(match(levels, value)[x], value, is.ordered(x))
}

