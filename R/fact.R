
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
#' @section level orders:
#'
#' The order of the levels may be adjusted to these rules depending on the class
#' of `x`:
#' \describe{
#'   \item{`character`}{The order of appearance}
#'   \item{`numeric`/`integer`/`Date`/`POSIXt`}{By the numeric order}
#'   \item{`logical`}{As `TRUE`, `FALSE`, then `NA` if present}
#'   \item{`factor`}{Numeric if levels can be safely converted, otherwise as
#'   they are}
#' }
#'
#' @param x A vector of values
#' @return A vector of equal length of `x` with class `fact` and `factor`.  If
#'   `x` was `ordered`, that class is added in between.
#'
#' @seealso [as_ordered()]
#' @family factors
#' @export
fact <- function(x) {
  UseMethod("fact", x)
}

#' @rdname fact
#' @export
fact.default <- function(x) {
  stop(
    "No fact method for class(es) ",
    collapse0(class(x), sep = ", "),
    call. = FALSE
  )
}

#' @rdname fact
#' @export
fact.character <- function(x) {
  out <- pseudo_id(x)
  new_fact(out, .uniques(out))
}

#' @rdname fact
#' @export
fact.numeric <- function(x) {
  u <- sort.int(unique(x), method = "radix", na.last = TRUE)

  # Don't bother NaN
  nas <- is.na(u)
  u[nas] <- NA

  if (sum(nas) > 1) {
    u <- u[-length(u)]
  }

  x[is.nan(x)] <- NA
  new_fact(match(x, u), u)
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

  new_fact(
    out,
    levels = c(TRUE, FALSE, if (any(nas)) NA),
    na = if (any(nas)) 3L else 0L
  )
}

#' @rdname fact
#' @export
fact.factor <- function(x) {
  old_levels <- levels(x)
  new_levels <- fact_coerce_levels(old_levels)

  if (is.logical(new_levels)) {
    m <- match(new_levels[x], c(TRUE, FALSE, NA))
    res <- new_fact(m, c(TRUE, FALSE, if (anyNA(new_levels[x])) NA))
    return(res)
  }

  if (is.numeric(new_levels) || inherits(x, c("Date", "POSIXt"))) {
    ord_levels <- sort(new_levels, na.last = TRUE)
    o <- match(old_levels, as.character(ord_levels))

    levels <- c(ord_levels, if (anyNA(x) && !anyNA(ord_levels)) NA)

    if (identical(o, seq_along(o))) {
      res <- new_fact(x, levels, is.ordered(x))
      return(res)
    }

    m <- match(order(old_levels), o)[x]
    res <- new_fact(m, levels, is.ordered(x))
    return(res)
  }

  if (anyNA(x) || anyNA(old_levels)) {
    new_levels <-
      if (!anyNA(new_levels)) {
        c(new_levels, NA)
      } else {
        na_last(new_levels)
      }
  }

  m <- match(old_levels, as.character(new_levels))[x]
  new_fact(m, new_levels, is.ordered(x))
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

  new_fact(x, levels = u)
}

#' @rdname fact
#' @export
fact.haven_labelled <- function(x) {
  require_namespace("haven")
  lvls <- attr(x, "labels")

  if (length(lvls)) {
    ux <- unclass(x)
    uniques <- sort.int(unique(c(ux, lvls)))
    m <- match(ux, uniques)
    ml <- match(lvls, uniques)
    uniques[ml] <- names(lvls)
    res <- new_fact(m, uniques)
  } else {
    res <- fact(unclass(x))
  }

  attr(res, "label") <- exattr(x, "label")
  res
}

#' @export
print.fact <- function(
  x,
  max_levels = getOption("mark.fact.max_levels", TRUE),
  width = getOption("width"),
  ...
) {
  # mostly a reformatted base::print.factor()
  ord <- is.ordered(x)
  if (length(x) == 0L) {
    cat(if (ord) "ordered" else "factor", "(0)\n", sep = "")
  } else {
    print(as.character(x), quote = FALSE, ...)
  }

  if (max_levels) {
    lev <- encodeString(levels(x), quote = "")
    n <- length(lev)
    colsep <- if (ord) " < " else " "
    T0 <- "Levels: " # nolint: object_name_linter.
    if (is.logical(max_levels)) {
      max_levels <- {
        width <- width - (nchar(T0, "w") + 3L +  1L + 3L)
        lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))

        if (n <= 1L || lenl[n] <= width) {
          n
        } else {
          max(1L, which.max(lenl > width) - 1L)
        }
      }
    }
    drop <- n > max_levels
    cat(
      if (drop) paste(format(n), ""),
      T0,
      paste(
        if (drop) {
          c(lev[1L:max(1, max_levels - 1)], "...", if (max_levels >  1) lev[n])
        } else {
          lev
        },
        collapse = colsep
      ),
      "\n",
      sep = ""
    )

    # Be nice to haven_labelled
    lab <- exattr(x, "label")
    if (!is.null(lab)) {
      cat("Label: ", paste(format(lab), ""), "\n", sep = "")
    }
  }

  invisible(x)
}

# as_ordered --------------------------------------------------------------

#' Ordered
#'
#' As ordered
#'
#' @details Simple implementation of `ordered`.  If `x` is `ordered` it is
#' simply returned.  If `x` is a `factor` the `ordered` class is added.
#' Otherwise, `x` is made into a `factor` with [mark::fact()] and then the
#' `ordered` class is added. Unlike just `fact`, `ordered` will replace the `NA`
#' levels with `NA_integer_` to work appropriately with other functions.
#'
#' @inheritParams fact
#' @seealso [fact()]
#' @family factors
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
  res <- fact_na(x, remove = TRUE)

  if (!is.ordered(x)) {
    res <- add_class(res, "ordered", 2L)
  }

  res
}


# drop_levels -------------------------------------------------------------

#' Drop levels
#'
#' Drop unused levels of a factor
#'
#' @param x A `factor` or `data.frame`
#' @param ... Additional arguments passed to methods (not used)
#' @seealso [base::droplevels]
#' @export
#' @family factors
drop_levels <- function(x, ...) {
  UseMethod("drop_levels", x)
}

#' @export
#' @rdname drop_levels
drop_levels.data.frame <- function(x, ...) {
  factors <- which(vap_lgl(x, is.factor))
  x[factors] <- lapply(x[factors], drop_levels)
  x
}

#' @export
#' @rdname drop_levels
drop_levels.fact <- function(x, ...) {
  if (is.ordered(x)) {
    as_ordered(fact_values(x))
  } else {
    fact(fact_values(x))
  }
}

#' @export
#' @rdname drop_levels
drop_levels.factor <- function(x, ...) {
  chr <- as.character(x)
  lvl <- levels(x) %wi% chr
  struct(
    match(chr, lvl),
    class = c(if (is.fact(x)) "fact", if (is.ordered(x)) "ordered", "factor"),
    levels = lvl
  )
}

# fact_na -----------------------------------------------------------------

#' `fact` with `NA`
#'
#' Included `NA` values into `fact()`
#'
#' @details
#' This re-formats the `x` value so that `NA`s are found immediately within the
#' object rather than accessed through its attributes.
#'
#' @param x A `fact` or object cohered to `fact`
#' @param remove If `TRUE` removes `NA` value from the `fact` `levels` and
#'   `uniques` attributes
#' @returns A `fact` vector
#' @family factors
#' @export
fact_na <- function(x, remove = FALSE) {
  x <- fact(x)
  na <- attr(x, "na")

  if (na == 0L) {
    return(x)
  }

  if (remove) {
    attr(x, "levels")  <- attr(x, "levels")[-na]
    attr(x, "uniques") <- attr(x, "uniques")[-na]
  }

  a <- attributes(x)
  x <- unclass(x)
  x[x == na] <- NA_integer_
  attributes(x) <- a
  attr(x, "na") <- 0L
  x
}

# fact_reverse ------------------------------------------------------------

#' Fact reverse levels
#'
#' Reverse the levels of a `fact`
#'
#' @param x A `fact` object (or passed to [fact()])
fact_reverse  <- function(x) {
  x <- fact(x)
  lvls <- flip(attr(x, "uniques"))
  seq <- flip(seq_along(lvls))
  na <- attr(x, "na")

  if (na > 0) {
    lvls <- c(lvls[-1L], lvls[1L])
    seq <- c(seq[-1L], seq[1L])
  }

  new_fact(seq[x], levels = lvls, ordered = is.ordered(x), na = na)
}

# other methods -----------------------------------------------------------

#' @export
as.integer.fact <- function(x, ...) {
  x <- fact_na(x)
  nas <- is.na(x)
  attributes(x) <- NULL
  class(x) <- "integer"
  x[nas] <- NA_integer_
  x
}

#' @export
as.double.fact <- function(x, ...) {
  as.double(as.integer(x))
}

#' @export
as.character.fact <- function(x, ...) {
  as.character(attr(x, "uniques")[x])
}

# because unique.factor() remakes factor
# this won't drop levels
#' @export
unique.fact <- function(x, incomparables = FALSE, ...) {
  att <- attributes(x)
  struct(
    unique(unclass(x)),
    class = att$class,
    levels = att$levels,
    uniques = att$uniques,
    na = att$na
  )
}

#' @export
as.Date.fact <- function(x, ...) {
  as.Date(attr(x, "uniques"), ...)[x]
}

#' @export
`[.fact` <- function(x, ...)  {
  y <- NextMethod("[")
  attributes(y) <- attributes(x)
  y
}

# helpers -----------------------------------------------------------------

new_fact <- function(
  x,
  levels,
  ordered = FALSE,
  na = if (anyNA(levels)) length(levels) else 0L
) {
  struct(
    as.integer(x),
    class = c("fact", if (ordered) "ordered", "factor"),
    levels = as.character(levels),
    uniques = levels,
    na = na
  )
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

  out <- rep.int(NA_real_, length(x))
  out[!nas] <- nums
  out
}

fact_values <- function(x) {
  if (!is.fact(x)) {
    stop("x must be a fact object", call. = FALSE)
  }

  attr(x, "uniques")[as.integer(x)]
}

fact_coerce_levels <- function(x) {
  nas <- is.na(x)

  if (all(nas) || !anyNA(match(x[!nas], c("TRUE", "FALSE")))) {
    return(as.logical(x))
  }

  tz <- getOption("mark.default_tz", "UTC")
  wuffle({
    numbers <- as.numeric(x[!nas])
    dates <- as.Date(x[!nas], optional = TRUE)
    posix <- as.POSIXct(
      x          = x[!nas],
      tryFormats = try_formats(),
      tz         = tz,
      optional   = TRUE
    )

  })

  n <- length(x)

  if (!anyNA(dates) && all(nchar(x[!nas]) == 10L)) {
    x <- rep(NA_Date_, n)
    x[!nas] <- dates
  } else if (!anyNA(posix)) {
    x <- rep(NA_real_, n)
    stopifnot(all(!nas))
    x[] <- as.double(posix)
    x <- as.POSIXct(
      x          = x,
      origin     = "1970-01-01",
      tz         = tz
    )
  } else if (!anyNA(numbers)) {
    x <- rep(NA_real_, n)
    x[!nas] <- numbers
  }

  x
}

`fact_levels<-` <- function(x, value) {
  x <- fact(x)
  levels <- levels(x)
  value <- c(value, if (!anyNA(value) & (anyNA(x) | anyNA(levels))) NA)
  new_fact(match(levels, value)[x], value, is.ordered(x))
}

is.fact <- function(x) { # nolint: object_name_linter.
  inherits(x, "fact")
}
