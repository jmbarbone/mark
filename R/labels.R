#' Dataframe labels
#'
#' Assign labels to a vector or data.frame.
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
#' @name labels
#' @export
#'
#' @examples
#' ## Best when run with RStudio
#'
#' labs <- assign_label(iris,
#'                      Sepal.Length = "cms",
#'                      Sepal.Width  = "cms",
#'                      Petal.Length = "cms",
#'                      Petal.Width  = "cms",
#'                      Species      = "Iris ...")
#' # View(labs)
#'
#' labs$dummy <- ""
#' get_labels(labs) # shows label as <NA> for dummy column
#' # view_labels(labs)
#'
#' labs0 <- remove_labels(labs, c("Sepal.Length", "Sepal.Width"))
#' get_labels(labs0) # No labels for Sepal.Length and Sepal.Width

assign_labels <- function(x, ...) {
  UseMethod("assign_labels", x)
}

#' @export
#' @rdname labels
assign_labels.default <- function(x, label, ...) {
  stopifnot("`label` is NULL" = !is.null(label),
            "`label` is not of length 1L" = length(label) == 1L)
  attr(x, "label") <- label
  x
}

#' @export
#' @rdname labels
assign_labels.data.frame <- function(x, ...) {
  ls <- list(...)

  if (is.null(ls) || any(vap_lgl(ls, is.null))) {
    stop("... must not have NULLs", call. = FALSE)
  }

  if (inherits(ls[[1]], "data.frame")) {
    lsx <- as.vector(ls[[1]][[2]], "list")
    names(lsx) <- ls[[1]][[1]]
    ls <- lsx
  }

  nm <- names(ls)
  ma <- match(nm, colnames(x), nomatch = NA_integer_)

  if (anyNA(ma)) {
    stop("Columns not found: ", collapse0(nm[is.na(ma)], sep = ", "), call. = FALSE)
  }

  for (i in seq_along(nm)) {
    mi <- ma[i]
    x[[mi]] <- assign_labels(x[[mi]], ls[[i]])
  }

  x
}

#' @export
#' @rdname labels
assign_label <- function(x, ...) {
  assign_labels(x, ...)
}


#' @export
#' @rdname labels
get_labels <- function(x) {
  UseMethod("get_labels", x)
}

#' @export
#' @rdname labels
get_labels.data.frame <- function(x) {
  vector2df(vap_chr(x, get_labels, .nm = TRUE), "column", "label")
}

#' @export
#' @rdname labels
get_labels.default <- function(x) {
  lb <- attr(x, "label")

  if (is.null(lb)) {
    return(NA_character_)
  }

  lb
}

#' @export
#' @rdname labels
view_labels <- function(x, title) {
  if (missing(title)) {
    title <- paste0(as.character(substitute(x)), " - Labels")
  }

  view_fun <- get("View", envir = as.environment("package:utils"))
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
