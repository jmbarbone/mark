#' Dataframe labels
#'
#' Assign labels to a vector or data.frame.
#'
#' @param x A vector of data.frame
#' @param ... One or more unquoted expressed separated by commas
#' @param label A single length string of a label to be assigned
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

assign_label <- function(x, ...) {
  UseMethod("assign_label", x)
}

#' @export
#' @rdname labels
assign_label.default <- function(x, label, ...) {
  stopifnot(length(label) == 1L)
  attr(x, "label") <- label
  x
}

#' @export
#' @rdname labels
assign_label.data.frame <- function(x, ...) {
  ls <- list(...)
  n <- names(ls)

  for(i in seq_along(n)) {
    x[[i]] <- assign_label(x[[i]], ls[[i]])
  }

  x
}

#' @export
#' @rdname labels
get_labels <- function(x) {
  stopifnot("`x` must be a data.frame" = inherits(x, "data.frame"))
  vector2df(vapply(unclass(x), attr, character(1), "label"), "column", "label")
}
