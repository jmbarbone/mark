#' Details an object
#'
#' Provides details about an object
#'
#' @param x An object
#' @param ... Additional arguments passed to methods
#' @examples
#' x <- sample(letters[1:4], 10, TRUE)
#' detail(x)
#'
#' df <- quick_df(list(
#'   x = x,
#'   y = round(runif(10), 2),
#'   z = Sys.Date() + runif(10) * 100
#' ))
#'
#' detail(df)
#' @export

detail <- function(x, ...) {
  UseMethod("detail", x)
}

#' @rdname detail
#' @param factor_n An `integer` threshold for making factors; will convert any
#'   character vectors with `factor_n` or less unique values into a `fact`;
#'   setting as `NA` will ignore this
#' @export
detail.default <- function(x, factor_n = 5L, ...) {
  stopifnot(!is.list(x))

  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op), add = TRUE)

  nas <- is.na(x)
  x2 <- x[!nas]
  facts <- is.factor(x)

  no_length <- length(x2) == 0L

  if (no_length) {
    quants <- FALSE
    nc <- NA
  } else {
    quants <- !is.character(x) && !facts
    nc <- nchar(as.character(x))
  }

  if (!is.na(factor_n) && !facts) {
    # If either of these exist, make as factor
    has_lls <-
      !is.null(exattr(x, "levels")) ||
      !is.null(exattr(x, "labels"))

    if (has_lls) {
      x <- fact(x)
    }
  }

  if (!no_length & !facts & !quants & length(unique(x)) <= 5) {
    x <- fact(x)
    facts <- TRUE
  }

  res <- quick_dfl(
    class = collapse0(class(x), sep = "; "),
    type  = collapse0(typeof(x), sep = "; "),
    label = exattr(x, "label") %||% NA_character_,
    n     = length(x2),
    na    = sum(nas),
    # These are a little funky
    min_c = as.character(min(if (quants) x2 else nc)) ,
    max_c = as.character(max(if (quants) x2 else nc))
  )

  if (facts) {
    res <- cbind(res, vector2df(counts(x), "level", "level_n"))
  } else {
    res$level <- NA_character_
    res$level_n <- NA_integer_
  }

  text <- quick_dfl(
    note    = note(x)    %||% NA_character_,
    comment = comment(x) %||% NA_character_
  )

  cbind(res, text)
}

#' @rdname detail
#' @export
detail.data.frame <- function(x, factor_n = 5L, ...) {
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op), add = TRUE)

  # remove list columns
  x <- x[, !vap_lgl(x, is.list), drop = FALSE]

  if (!ncol(x)) {
    stop("x does not have any non-list columns", call. = FALSE)
  }

  details <- lapply(x, detail, factor_n = factor_n)
  reps <- vap_int(details, nrow)

  cbind(
    quick_dfl(i = rep(seq_along(x), reps)),
    quick_dfl(col = rep(names(x), reps)),
    Reduce(rbind, details)
  )
}
