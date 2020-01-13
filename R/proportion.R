#' Calculate a proportion
#'
#' Calculates a proportion!
#'
#' @param x A data.frame or a vector.
#' @param .name The name of the column or new name for the group.  For data.frames this must be set.
#' @param ... Additional arguments to be passed to methods (not in use).
#'
#' @examples
#' proportion(iris, Species)
#' proportion(iris$Species)
#' proportion(iris$Species)
#'
#' @export

proportion <- function(x, .name) {
  UseMethod("proportion")
}

#' @export
proportion.default <- function(x, .name = NULL) {
  groups <- sort(unique(x))
  res <- vapply(groups, function(.x) mean(.x == x), double(1), USE.NAMES = T)
  names(res) <- groups
  res
}

#' @export
proportion.tbl <- function(x, .name) {
  col <- as.character(substitute(.name))

  groups <- sort(unique(x[[col]]))
  props <- vapply(groups, function(x) mean(x[[col]]), double(1), USE.NAMES = T)

  tibble::enframe(props, col, "prop")
}

#' @export
proportion.data.frame <- function(x, .name) {

  if(is.name(substitute(.name))) {
    vals <- eval(substitute(.name), x, enclos = parent.frame())
    col_name <- deparse(substitute(.name))
  } else {
    vals <- x[[.name]]
    col_name <- .name
  }

  groups <- sort(unique(vals))

  res <- data.frame(
    col_name = groups,
    props = vapply(groups, function(.x) mean(.x == vals), double(1))
  )

  names(res)[1] <- col_name
  res
}


