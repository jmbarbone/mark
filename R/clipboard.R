#' Write to and read from the clipboard
#'
#' Wrappers for working with the clipboard
#'
#' @details As these functions rely on [utils::readClipboard()] and
#' [utils::writeClipboard()] they are only available for Windows 10. For copying
#' and pasting floats, there may be some rounding that can occur.
#'
#' @param x An object
#' @param method Method switch for loading the clipboard
#' @param ... Additional arguments sent to methods or to [utils::write.table()]
#'
#' @return `write_clipboard()` None, called for side effects `read_clipboard()`
#' Either a vector, `data.frame`, or `tibble` depending on the `method` chosen.
#' Unlike [utils::readClipboard()], an empty clipboard value returns `NA` rather
#' than `""`
#'
#' @name clipboard
#' @examples
#' # Will only run on windows
#' if (Sys.info()[["sysname"]] == "Windows") {
#'   foo <- function(x) {
#'     write_clipboard(x)
#'     y <- read_clipboard()
#'     res <- all.equal(x, y)
#'     if (isTRUE(res)) return("All equal")
#'     print(x)
#'     print(y)
#'   }
#'   foo(1:4)
#'   foo(seq(-1, 1, .02))
#'   foo(Sys.Date() + 1:4)
#'
#'   # May have some rounding issues
#'   x <- "0.316362437326461129"
#'   write_clipboard(x)
#'   res <- as.character(read_clipboard())
#'   all.equal(x, res)
#'   x; res
#' }

# nocov start

#' @export
#' @rdname clipboard
write_clipboard <- function(x, ...) {
  clear_clipboard()
  UseMethod("write_clipboard", x)
}

#' @export
#' @rdname clipboard
write_clipboard.default <- function(x, ...) {
  utils::writeClipboard(str = as.character(x), format = 1L)
}

#' @export
#' @rdname clipboard
#' @inheritParams utils::write.table
write_clipboard.data.frame <- function(x, sep = "\t", row.names = FALSE, ...) { # nolint: object_name_linter, line_length_linter.
  utils::write.table(
    x,
    file = "clipboard-128",
    sep = sep,
    row.names = row.names,
    ...
  )
}

#' @export
#' @rdname clipboard
write_clipboard.matrix <- function(x, sep = "\t", ...) {
  write_clipboard.data.frame(x, sep = sep, ...)
}

#' @export
#' @rdname clipboard
write_clipboard.list <- function(x, sep = "\t", ...) {
  ls <- list2df(x)
  write_clipboard(ls, sep = "\t", ...)
}

#' @export
#' @rdname clipboard
read_clipboard <- function(method = c("default", "data.frame", "tibble"), ...) {
  if (!is_windows()) {
    stop(cond_read_clipboard_windows())
  }

  switch(
    match_param(method),

    default = {
      x <- utils::readClipboard(format = 1L, raw = FALSE)
      type_convert2(x)
    },

    # Specifications I prefer -- mostly copying from Excel
    data.frame = {
      tab <- do_read_table_clipboard(...)

      for (i in seq_along(tab)) {
        tab[[i]] <- type_convert2(tab[[i]])
      }

      tab
    },

    tibble = {
      require_namespace("tibble")
      tibble::as_tibble(read_clipboard("data.frame", ...))
    }
  )
}

# helpers -----------------------------------------------------------------

#' Read table from clipboard
#'
#' A wrapper for reading a table
#'
#' @inheritParams utils::read.table
#' @noRd
# nolint start: object_name_linter.
do_read_table_clipboard <- function(
    header           = TRUE,
    # Copying form Excel produces tab separations
    sep              = "\t",
    row.names        = NULL,
    # Excel formula for NA produces #N/A -- sometimes people use N/A...
    na.strings       = c("", "NA", "N/A", "#N/A"),
    check.names      = FALSE,
    stringsAsFactors = FALSE,
    encoding         = "UTF-8",
    # occasionally "#' is used as a column name -- may cause issues
    comment.char     = "",
    blank.lines.skip = FALSE,
    fill             = TRUE,
    ...
    # nolint end: objecT_name_linter.
) {
  utils::read.table(
    file             = "clipboard-128",
    header           = header,
    sep              = sep,
    row.names        = row.names,
    na.strings       = na.strings,
    check.names      = check.names,
    stringsAsFactors = stringsAsFactors,
    encoding         = encoding,
    comment.char     = comment.char,
    blank.lines.skip = blank.lines.skip,
    fill             = fill,
    ...
  )
}

clear_clipboard <- function() {
  if (!is_windows()) {
    stop("`mark::write_clipboard()` is only valid for Windows", call. = FALSE)
  }

  utils::writeClipboard("", format = 1L)
}

# nocov end

# coverage ----------------------------------------------------------------

type_convert2 <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  res <- utils::type.convert(
    x,
    as.is = TRUE,
    numerals = "warn.loss",
    dec = getOption("OutDec", ".")
  )

  if (is.character(res)) {
    x0 <- x[trimws(x) != "" & !is.na(x)]
    n <- length(x0)

    if (!n) {
      return(res)
    }

    x0 <- trimws(x0[1:min(n, 1000)])

    if (all(toupper(x0) %in% c("TRUE", "FALSE", "NA"))) {
      return(as.logical(toupper(x)))
    }

    dates <- as_date_strptime(x0)

    # if (isTRUE(all.equal(as.character(dates), x0))) {
    if (!anyNA(dates)) {
      return(as_date_strptime(x))
    }
  }

  res
}

# conditions --------------------------------------------------------------

cond_read_clipboard_windows <- function() {
  new_condition(
    "`mark::read_clipboard()` is only valid for Windows",
    "read_clipboard_windows"
  )
}
