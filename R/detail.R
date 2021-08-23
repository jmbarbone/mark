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
#' df <- quick_dfl(
#'   x = x,
#'   y = round(runif(10), 2),
#'   z = Sys.Date() + runif(10) * 100
#' )
#'
#' detail(df)
#' @export

detail <- function(x, ...) {
  UseMethod("detail", x)
}

#' @rdname detail
#' @export
detail.default <- function(x, ...) {
  stopifnot(!is.list(x))

  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op), add = TRUE)

  nas <- is.na(x)
  x2 <- x[!nas]

  # If either of these exact, make as factor
  has_lls <-
    !is.null(attr(x, "levels", exact = TRUE)) ||
    !is.null(attr(x, "labels", exact = TRUE))

  if (has_lls) {
    x <- fact(x)
  }

  facts <- is.factor(x)
  quants <- !is.character(x) && !facts
  nc <- nchar(as.character(x))

  if (!facts & !quants) {
    if (length(unique(x)) <= 5) {
      x <- fact(x)
      facts <- TRUE
    }
  }

  res <- quick_dfl(
    class = collapse0(class(x), sep = "; "),
    type  = collapse0(typeof(x), sep = "; "),
    label = attr(x, "label", exact = TRUE) %||% NA_character_,
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
detail.data.frame <- function(x, ...) {
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op), add = TRUE)

  # remove list columns
  x <- x[, !vap_lgl(x, is.list)]

  if (!ncol(x)) {
    stop("x does not have any non-list columns", call. = FALSE)
  }

  details <- lapply(x, detail)
  reps <- vap_int(details, nrow)

  cbind(
    quick_dfl(i = rep(seq_along(x), reps)),
    quick_dfl(col = rep(names(x), reps)),
    Reduce(rbind, details)
  )
}
