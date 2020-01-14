#' Source file from directory
#'
#' Walk through files in a directory and output them
#'
#' @param dir The location of your R scripts
#' @param quiet Logical.  Whether to print out a message for each file.
#' @inheritParams base::source
#' @param ... Additional arguments passed to [base::source()]
#'
#' @examples
#' \dontrun{
#' source_dir_r("C:/Users/jbarbone/GitHub/test")
#' }
#' @export

source_dir_r <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  invisible(lapply(list.files(dir, pattern = "\\.R$", full.names = T),
                   function(x, q) {
                     source(x, echo = echo, ...)
                     if(!q) message(sprintf("Successfully sourced: %s", x))
                     invisible()},
                   q = quiet))
}
