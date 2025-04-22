#' Lines of R code
#'
#' Find the total number of lines of R code
#'
#' @details
#' Tries to read each file in the directory that ends in .R or .r and sums
#'   together.  Files that fail to read are not counted.
#'
#' @param x Directory to search for files
#' @param skip_empty Logical, if TRUE will not count lines that are empty or
#'   only contain a bracket or quotation mark.
#'
#' @returns An `integer` for the number of lines in all applicable files
#' @export
#'
#' @examples
#' \donttest{
#' lines_of_r_code(system.file())
#' lines_of_r_code(system.file(), skip_empty = FALSE)
#' }

lines_of_r_code <- function(x = ".", skip_empty = TRUE) {
  if (dir.exists(x)) {
    x <- list_r_files(x)
  }

  if (skip_empty) {
    return(sum(vap_int(x, n_lines_r_file)))
  }

  sum(vap_int(x, n_lines_r_file_all))
}

list_r_files <- function(x = ".") {
  list_files(x, pattern = "\\.[rR]$", all = TRUE)
}

n_lines_r_file <- function(r_file) {
  withCallingHandlers(
    {
      x <- trimws(readLines(r_file, skipNul = TRUE, warn = FALSE))
      length(x %wo% c("", "}", "{", "(", ")", "[", "]", '"', "'"))
    },
    error = function(e) 0L
  )
}

n_lines_r_file_all <- function(r_file) {
  withCallingHandlers(
    length(readLines(r_file, skipNul = TRUE, warn = FALSE)),
    error = function(e) 0L
  )
}
