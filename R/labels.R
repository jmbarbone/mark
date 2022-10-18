#' Dataframe labels
#'
#' Assign labels to a vector or data.frame.
#'
#' @details
#' When labels are assigned to a data.frame they can make viewing the object
#'  (with `View()` inside Rstudio).  The `view_labels()` has a call to `View()`
#'  inside and will retrieve the labels and show them in the viewer as a
#'  data.frame.
#'
#' @param x A vector of data.frame
#' @param ... One or more unquoted expressed separated by commas.  If assigning
#'   to a data.frame, `...` can be replaced with a `data.frame` where the first
#'   column is the targeted colname and the second is the desired label.
#' @param label A single length string of a label to be assigned
#' @param cols A character vector of column names; if missing will remove the
#'   label attribute across all columns
#' @param title Title for the viewer window -- if not supplemented will show as
#'   `paste0(as.character(substitute(x)), " - Labels")`
#'
#' @return A labelled vector or `data.frame`
#' @name labels
#' @export
#'
#' @examples
#' labs <- assign_labels(
#'   iris,
#'   Sepal.Length = "cms",
#'   Sepal.Width  = "cms",
#'   Petal.Length = "cms",
#'   Petal.Width  = "cms",
#'   Species      = "Iris ..."
#' )
#'
#' labs$dummy <- ""
#' get_labels(labs) # shows label as <NA> for dummy column
#'
#' labs0 <- remove_labels(labs, c("Sepal.Length", "Sepal.Width"))
#' get_labels(labs0) # No labels for Sepal.Length and Sepal.Width

assign_labels <- function(x, ...) {
  UseMethod("assign_labels", x)
}

#' @export
#' @rdname labels
assign_labels.default <- function(x, label, ...) {
  if (is.null(label)) {
    stop("`label` is NULL", call. = FALSE)
  }

  if (length(label) != 1L) {
    stop("`label` is not of length 1L", call. = FALSE)
  }

  attr(x, "label") <- label
  x
}

#' @export
#' @rdname labels
#' @param .missing A control setting for dealing missing columns in a list;
#'   can be set to `error` to `stop()` the call, `warn` to provide a warning, or
#'   `skip` to silently skip those labels.
#' @param .ls A named list of columns and labels to be set if `...` is empty
assign_labels.data.frame <- function(x, ..., .missing = c("error", "warn", "skip"), .ls = list(...)) {
  # TODO add test for implementing assign_labels.data.frame(.missing = )

  if (identical(.ls, list())) {
    stop(".ls cannot be empty", call. = FALSE)
  }

  if (...length() & !identical(list(...), .ls)) {
    stop("... was set separately from `.ls`. Only set one", call. = FALSE)
  }

  .missing <- match_param(.missing)

  if (is.null(.ls) || any(vap_lgl(.ls, is.null))) {
    stop("... must not have NULLs", call. = FALSE)
  }

  if (inherits(.ls[[1]], "data.frame")) {
    lsx <- as.vector(.ls[[1]][[2]], "list")
    names(lsx) <- .ls[[1]][[1]]
    .ls <- lsx
  }

  nm <- names(.ls)
  ma <- match(nm, colnames(x), nomatch = NA_integer_)

  if (anyNA(ma)) {
    nas <- is.na(ma)
    text <- paste0("Columns not found: ", collapse0(nm[nas], sep = ", "))

    if (.missing == "error") {
      stop(text, call. = FALSE)
    }

    if (.missing == "warn") {
      warning(text, call. = FALSE)
    }

    nm  <-  nm[!nas]
    ma  <-  ma[!nas]
    .ls <- .ls[!nas]
  }

  for (i in seq_along(nm)) {
    mi <- ma[i]
    x[[mi]] <- assign_labels(x[[mi]], .ls[[i]])
  }

  x
}

#' @export
#' @rdname labels
get_labels <- function(x) {
  UseMethod("get_labels", x)
}

#' @export
#' @rdname labels
get_labels.default <- function(x) {
  exattr(x, "label") %||% NA_character_
}

#' @export
#' @rdname labels
get_labels.data.frame <- function(x) {
  vector2df(vap_chr(x, get_labels, .nm = TRUE), "column", "label")
}

#' @export
#' @rdname labels
view_labels <- function(x, title) {
  cesx <- charexpr(substitute(x))

  if (missing(title)) {
    title <- paste0(cesx, " - Labels")
  }

  pf <- parent.frame()
  view_fun <- get0("View", envir = pf, ifnotfound = "utils" %colons% "View")

  if (!is.function(view_fun)) {
    stop("Something went wrong trying to use `View()`",
         "\nThis may be because you are using Rstudio :",
         "\n  https://community.rstudio.com/t/view-is-causing-an-error/75297/4",
         "\nYou can try :\n  ",
         sprintf('`View(get_labels(%s), title = "%s")`',
                 cesx,
                 title),
         call. = FALSE)
  }

  view_fun(x = get_labels(x), title = title)
}

#' @export
#' @rdname labels
remove_labels <- function(x, ...) {
  UseMethod("remove_labels", x)
}

#' @export
#' @rdname labels
remove_labels.default <- function(x, ...) {
  attr(x, "label") <- NULL
  x
}

#' @export
#' @rdname labels
remove_labels.data.frame <- function(x, cols, ...) {
  if (missing(cols)) {
    cols <- seq_along(x)
  } else {
    bad <- cols %out% colnames(x)

    if (any(bad)) {
      stop("Column not found in data.frame:\n  ",
           collapse0(cols[bad], sep = ", "),
           call. = FALSE)
    }
  }

  for (i in cols) {
    x[[i]] <- remove_labels(x[[i]])
  }

  x
}
