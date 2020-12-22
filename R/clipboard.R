#' Write to and read from the clipboard
#'
#' Wrappers for working with the clipboard
#'
#' @param x An object
#' @param method Method switch for loading the clipboard
#' @param ... Additional arguments sent to methods
#'
#' @name clipboard

#' @export
#' @rdname clipboard
write_clipboard <- function(x = .Last.value, ...) {
  clear_clipboard()
  UseMethod("write_clipboard", x)
}

#' @export
write_clipboard.default <- function(x = .Last.value, format = 1L, ...) {
  utils::writeClipboard(str = x, format = format, ...)
}

#' @export
write_clipboard.data.frame <- function(x = .Last.value, sep = "\t", ...) {
  utils::write.table(x, file = "clipboard-128", sep = sep, row.names = FALSE, ...)
}

#' @export
write_clipboard.matrix <- function(x, sep = "\t", ...) {
  write_clipboard.data.frame(x, sep = sep, ...)
}

#' @export
write_clipboard.list <- function(x, sep = "\t", show_NA = FALSE, ...) {
  ls <- list2df(x, show_NA = show_NA)
  write_clipboard(ls, sep = "\t", ...)
}

#' @export
#' @rdname clipboard
read_clipboard <- function(method = c("default", "data.frame", "tibble"), ...) {
  switch(
    match_param(method),

    default = utils::readClipboard(),

    # Specifications I prefer -- mostly copying from Excel
    data.frame = utils::read.table(
      file = "clipboard-128",
      header = TRUE,
      # Copying form Excel produces tab sepertions
      sep = "\t",
      row.names = NULL,
      # Excel formula for NA produces #N/A -- sometimes people use N/A...
      na.strings = c("", "NA", "N/A", "#N/A"),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      # occasionally "#' is used as a column name -- may cause issues
      comment.char = "",
      blank.lines.skip = FALSE,
      fill = TRUE,
       ...
    ),

    tibble = tibble::as_tibble(read_clipboard("data.frame", ...))
    )
}

clear_clipboard <- function() {
  utils::writeClipboard("")
}
