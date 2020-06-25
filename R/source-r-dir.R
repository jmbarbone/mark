#' Source file from directory
#'
#' Walk through files in a directory and output them.
#' Files are sources in order of names
#'
#' @param dir The location of your R scripts
#' @param quiet Logical.  Whether to print out a message for each file.
#' @param path The location of the R file.
#' @inheritParams base::source
#' @param ... Additional arguments passed to [base::source()]
#'
#' @examples
#' \dontrun{
#' source_dir_r("C:/Users/jbarbone/GitHub/test")
#' }
#' @export
#' @name source_files

source_r_dir <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  files <- list.files(dir, pattern = "\\.[rR]$", full.names = TRUE)
  invisible(lapply(sort(files), source_r_file, q = quiet, ...))
}

#' @export %>%
#' @rdname source_files
source_dir_r <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  warning("Use `jordan::source_r_dir()` instead.",  call. = FALSE)
  source_r_dir(dir, echo, quiet, ...)
}

#' @export
#' @rdname source_files
#' @inheritParams source_files
source_r_file <- function(path, echo = FALSE, quiet = FALSE, ...) {
  stopifnot("Must be a .R file" = grepl("\\.[rR]$", path))
  if(!file.exists(path)) {
    stop(sprintf("File << %s >> not found.", path), call. = FALSE)
  }
  st <- system.time(source(path, echo = echo, ..., chdir = FALSE))
  if(!quiet) {
    message(sprintf("Successfully sourced: %s [%s]",
                    basename(path),
                    round(st[["elapsed"]], 2)))
  }
  invisible()
}

globalVariables(c("source_file_r", "quiet"))
