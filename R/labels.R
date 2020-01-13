#' Dataframe labels
#'
#' Assign labels to data frames
#'
#' @param .data A dataframe
#' @param ... One or more unquoted expressed separated by commas
#'
#' @importFrom Hmisc label
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

assign_label <- function(.data, ...) {
  ls <- list(...)
  n <- names(ls)

  for(i in seq_along(n)) {
    Hmisc::label(.data[[n[i]]]) <- ls[[i]]
  }

  .data
}
