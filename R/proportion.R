#' Proportions
#'
#' Calculates a proportion from a vector or data.frame/matrix.
#'
#' @details
#' tapply(x, x, length) / length(x) can be speedier for smaller data sets and groups
#'   but slows down with more data or a greater number of groups.
#'
#' @param x A data.frame or a vector.
#' @param ... Additional arguments to be passed to methods.
#' @param col A character string of the column name which holds the groups
#'
#' @examples
#' proportion(iris, "Species")
#' proportion(iris$Species)
#'
#'\dontrun{
#' ## With smaller n, tapply edges out as faster
#' microbenchmark::microbenchmark(
#'   tapply = prop_tapply(iris, "Sepal.Length"),
#'   proportion = proportion(iris, "Sepal.Length"),
#'   times = 1000
#' )
#' microbenchmark::microbenchmark(
#'   tapply = prop_tapply(iris$Species),
#'   proportion = proportion(iris$Species),
#'   times = 1000
#' )
#'
#' ## But in larger n, proportion is faster
#' x <- dplyr::sample_n(iris, 1e6, TRUE)
#' microbenchmark::microbenchmark(
#'   tapply = prop_tapply(x, "Sepal.Length"),
#'   proportion = proportion(x, "Sepal.Length"),
#'   times = 10
#' )
#' microbenchmark::microbenchmark(
#'   tapply = prop_tapply(x$Species),
#'   proportion = proportion(x$Species),
#'   times = 100
#' )
#'
#' ## more groups (very slow) prop_tapply excels
#' x <- runif(1e5)
#' microbenchmark::microbenchmark(
#'   tapply = prop_tapply(x),
#'   proportion = proportion(x),
#'   times = 2
#' )
#' }
#' @export

proportion <- function(x, ...) {
  UseMethod("proportion", x)
}

#' @export
proportion.default <- function(x, ...) {
  gr <- unique(x)
  res <- vapply(gr, function(.x) mean(.x == x), double(1))
  names(res) <- gr
  res
}

#' @export
proportion.data.frame <- function(x, col, ...) {
  vals <- x[[col]]
  col_name <- col

  groups <- unique(vals)

  res <- data.frame(
    col_name = groups,
    props = vapply(groups, function(.x) mean(.x == vals), double(1)),
    stringsAsFactors = FALSE,
    fix.empty.names = FALSE
  )

  names(res)[1] <- col_name
  res
}

#' @export
#' @rdname proportion
prop_tapply <- function(x, ...) {
  UseMethod("prop_tapply", x)
}

#' @export
#' @rdname proportion
prop_tapply.default <- function(x, ...) {
  tapply(x, x, length) / length(x)
}

#' @export
#' @rdname proportion
prop_tapply.data.frame <- function(x, col, ...){
  tapply(x[,col], x[,col], length) / nrow(x)
}

#' @export
#' @rdname proportion
cum_prop <- function(x) {
  x <- cumsum(x)
  x / x[length(x)]
}
