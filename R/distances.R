#' Add Mahalanobis distance
#'
#' Add a mahalanobis distance to the end of a data frame.
#'
#' @param df A data.frame
#' @param ... Columns to select
#' @param .inverted Logical. If TRUE, covariance matrix (p x p) of the distribution is supposed to contain the inverse of the covariance matrix.
#' @param .name The name of the new column for the distance value
#' @param .p The name for the new column for the p-value calculation
#'
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom tibble deframe
#' @importFrom stats mahalanobis
#' @importFrom stats cov
#'
#' @export
#'
#' @examples
#' add_mahalanobis(iris, Sepal.Length, Sepal.Width)

add_mahalanobis <- function(df, ..., .inverted = FALSE, .name = "md", .p = "p_value") {
  x <- dplyr::select(df, ...)
  df[[.name]] <- mahalanobis(x, tibble::deframe(dplyr::summarise_all(x, mean)), cov(x), interted = .inverted)
  df[[.p]] <- pchisq(df[[.name]], df = ncol(x), lower.tail = FALSE)
  df
}

#' Add Euclidean distance
#'
#' Add a Euclidean distance measure to the end of a data frame
#'
#' @inheritParams add_mahalanobis
#' @param .x,.y The two values to compute the distance from
#'
#' @export
#'
#' @examples
#' add_euclidean(iris, "Sepal.Width", "Sepal.Length")


add_euclidean <- function(df, .x, .y, .name = "eucd") {
  x1 <- df[[.x]]
  y1 <- df[[.y]]
  center <- vapply(list(x1, y1), mean, double(1), na.rm = T)
  eucs <- mapply(function(x, y) sqrt(sum((c(x, y) - center)^2)), x1, y1)
  df[[.name]] <- eucs
  df
}
