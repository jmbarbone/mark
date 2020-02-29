#' Write to and read from the clipboard
#'
#' Wrappers for working with the clipboard
#'
#' @param x An object
#' @param method Method switch for loading the clipboard
#' @param ... Additional arguments sent to methods
#'
#' @importFrom utils writeClipboard
#' @importFrom utils readClipboard
#' @importFrom utils read.table
#' @importFrom utils write.table
#'
#' @name clipboard

#' @export
#' @rdname clipboard
write_clipboard <- function(x, ...) {
  UseMethod("write_clipboard", x)
}

#' @export
write_clipboard.default <- function(x, format = "1L") {
  writeClipboard(str = x, format = format)
}

#' @export
write_clipboard.data.frame <- function(x, sep = "\t") {
  write.table(x, file = "clipboard", sep = sep, row.names = FALSE)
}

#' @export
write_clipboard.matrix <- function(x, sep = "\t") {
  write_clipboard.data.frame(x, sep = sep)
}
write_clipboard.list <- function(x, sep = "\t", show_NA = FALSE) {
  ls <- list2df(x, show_NA = show_NA)
  write_clipboard(ls, sep = "\t")
}


#' @export
#' @rdname clipboard
read_clipboard <- function(method = c("default", "data.frame", "tibble"), ...) {
  switch(match.arg(method),
         default = readClipboard(),
         `data.frame` = read.table(file = "clipboard", sep = "\t", ...),
         tibble = {
           require_namespace("tibble")
           tibble::as_tibble(read_clipboard("data.frame",
                                            row.names = NULL,
                                            stringsAsFactors = FALSE),
                             ...)
         })
}


# read.table(file = "clipboard", sep = "\t")
# tibble::as_tibble(readClipboard(), sep = "\t")



# global variables ----------------------------------------------------------------------------

globalVariables(c('readClipboard'))
