#' Write to and read from the clipboard
#'
#' Wrappers for working with the clipboard
#'
#' @details
#' As these functions rely on `utils::readClipboard()` and
#'   `utils::writeClipboard` they are only available for Windows 10.
#' For copying and pasting floats, there may be some rounding that can occur.
#'
#' @param x An object
#' @param method Method switch for loading the clipboard
#' @param ... Additional arguments sent to methods
#'
#' @name clipboard
#' @examples
#' \dontrun{
#'
#' foo <- function(x) {
#'   write_clipboard(x)
#'   y <- read_clipboard()
#'   res <- all.equal(x, y)
#'   if (isTRUE(res)) return("All equal")
#'   print(x)
#'   print(y)
#' }
#' foo(1:4)
#' foo(seq(-1, 1, .02))
#' foo(Sys.Date() + 1:4)
#'
#' # May have some rounding issues
#' x <- "0.316362437326461129"
#' write_clipboard(x)
#' res <- as.character(read_clipboard())
#' all.equal(x, res)
#' x; res
#' }

#' @export
#' @rdname clipboard
write_clipboard <- function(x, ...) {
  clear_clipboard()
  UseMethod("write_clipboard", x)
}

#' @export
write_clipboard.default <- function(x, ...) {
  try_write_clipboard(str = as.character(x))
}

#' @export
write_clipboard.data.frame <- function(x, sep = "\t", ...) {
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

  stopifnot("`jordan::read_clipboard()` is only valid for Windows" = is_windows())

  switch(
    match_param(method),

    default = {
      x <- try_read_clipboard()
      x <- try_vector_formats(x)
      x <- try_vector_formats(x)
      try_vector_formats(x)
    },

    # Specifications I prefer -- mostly copying from Excel
    data.frame = {
      tab <- utils::read.table(
        file = "clipboard-128",
        header = TRUE,
        # Copying form Excel produces tab separations
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
      )

      for (i in seq_along(tab)) {
        tab[[i]] <- try_vector_formats(tab[[i]])
      }

      tab
      },

    tibble = tibble::as_tibble(read_clipboard("data.frame", ...))
    )
}

clear_clipboard <- function() {
  stopifnot("`jordan::write_clipboard()` is only valid for Windows" = is_windows())
  try_write_clipboard("")
}

try_vector_formats <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  # Take subset to determine
  x0 <- x[x != ""]
  n <- length(x0)

  if (n == 0L) {
    return(x)
  }

  x0 <- trimws(x0[1:min(n, 1000)])
  if (all(toupper(x0) %in% c("TRUE", "FALSE", "NA"))) {
    return(as.logical(toupper(x)))
  }

  dbls <- wuffle(as.double(x0))
  dbls <- dbls[!is.nan(dbls)]

  if (is_length0(dbls)) {
    return(x)
  }

  if (!anyNA(dbls)) {
    if (isTRUE(all.equal(dbls, as.integer(dbls)))) {
      return(as.integer(x))
    }

    return(as.double(x))
  }

  dates <- as_date_strptime(x0)
  if (isTRUE(all.equal(as.character(dates), x0))) {
    return(dates)
  }

  x
}

is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

try_read_clipboard <- function(tries = 1L) {
  tryCatch(utils::readClipboard() %||% warning(),
  warning = function(e) {
    if (tries >= 10) {
      stop("Failure to read clipboard after 10 attempts", call. = FALSE)
    }
    try_read_clipboard(tries + 1L)
  }
  )
}

try_write_clipboard <- function(str, tries = 1L) {
  tryCatch(
    utils::writeClipboard(str, format = 1L),
    warning = function(e) {
      if (tries >= 10) {
        stop("Failure to write clipboard after 10 attempts", call. = FALSE)
      }
      try_write_clipboard(str, tries + 1L)
    }
  )
}
