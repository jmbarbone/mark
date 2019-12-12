#' Source file from directory
#'
#' Walk through files in a directory and output them
#'
#' @param dir The location of your R scripts
#' @param quiet Logical.  Whether to print out a message for each file.
#'
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' source_dir_r("C:/Users/jbarbone/GitHub/easyr/R/")
#' }
#' @export

source_dir_r <- function(dir, quiet = F) {
  invisible(purrr::map(list.files(dir, pattern = "\\.R$", full.names = T),
                       function(x, q) {
                         source(x, echo = F)
                         if(!q) message(sprintf("Successfully sourced: %s", x))
                         invisible()},
                       q = quiet))
}
