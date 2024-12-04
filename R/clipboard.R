#' Write to and read from the clipboard
#'
#' Wrappers for working with the clipboard
#'
#' @details As these functions rely on [clipr::read_clip()] and
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
  x <- as.character(x)
  clipr::write_clip(x, allow_non_interactive = TRUE)
}

#' @export
#' @rdname clipboard
#' @inheritParams utils::write.table
# nolint next: object_name_linter.
write_clipboard.data.frame <- function(x, sep = "\t", row.names = FALSE, ...) {
  fuj::require_namespace("clipr")
  clipr::write_clip(
    content = x,
    allow_non_interactive = TRUE,
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
read_clipboard <- function(method = read_clipboard_methods(), ...) {
  fuj::require_namespace("clipr")
  switch(
    match_param(method),
    default = type_convert2(clipr_read_clip(TRUE)),
    tibble = ,
    excel = ,
    calc = ,
    # default are specifications for excel/calc
    data.frame = type_convert2(do_read_table_clipboard(...)),
    csv = read_clipboard("data.frame", sep = ",", ...),
    csv2 = read_clipboard("data.frame", sep = ";", ...),
    bsv = ,
    psv = read_clipboard("data.frame", sep = "|", ...),
    tsv = read_clipboard("data.frame", sep = "\t", ...),
    md = {
      fuj::require_namespace("readMDTable")
      temp <- fs::file_temp()
      on.exit(fs::file_delete(temp), add = TRUE)
      writeLines(read_clipboard(), temp)
      params <- list0(...)
      params$file <- temp
      params$show_col_types <- params$show_col_types %||% FALSE
      params$col_types <- params$col_types %||% list(.default = "character")
      type_convert2(do.call(readMDTable::read_md_table, params))
    }
  )
}

clipr_read_clip <- function(...) {
  res <- withCallingHandlers(
    clipr::read_clip(...),
    simpleWarning = function(e) {
      if (grepl(
        "System clipboard contained no readable text",
        conditionMessage(e),
        fixed = TRUE
      )) {
        tryInvokeRestart("muffleWarning")
      }
    }
  )

  if (is_windows() && (isNA(res) || (length(res) == 1L && !nzchar(res)))) {
    NULL
  } else {
    res
  }
}

#' @export
#' @rdname clipboard
read_clipboard_methods <- function() {
  c(
    "default",
    "data.frame",
    "tibble",
    "excel",
    "calc",
    "csv",
    "csv2",
    "bsv",
    "psv",
    "tsv",
    "md",
    NULL
  )
}

# helpers -----------------------------------------------------------------

#' Read table from clipboard
#'
#' A wrapper for reading a table
#'
#' @inheritParams utils::read.table
#' @noRd
do_read_table_clipboard <- function(
    header           = TRUE,
    # Copying form Excel produces tab separations
    sep              = "\t",
    # nolint next: object_name_linter.
    row.names        = NULL,
    # Excel formula for NA produces #N/A -- sometimes people use N/A...
    # nolint next: object_name_linter.
    na.strings       = c("", "NA", "N/A", "#N/A"),
    # nolint next: object_name_linter.
    check.names      = FALSE,
    # nolint next: object_name_linter.
    stringsAsFactors = FALSE,
    encoding         = "UTF-8",
    # occasionally "#' is used as a column name -- may cause issues
    # nolint next: object_name_linter.
    comment.char     = "",
    # nolint next: object_name_linter.
    blank.lines.skip = FALSE,
    fill             = TRUE,
    ...
) {
  res <- utils::read.table(
    file = textConnection(clipr_read_clip(TRUE)),
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

  if (package_available("tibble")) {
    tibble::as_tibble(res)
  } else {
    res
  }
}

clear_clipboard <- function() {
  clipr::clear_clip(allow_non_interactive = TRUE)
}

# nocov end

# coverage ----------------------------------------------------------------

type_convert2 <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, type_convert2)
    return(x)
  }

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

    if (!anyNA(dates)) {
      return(as_date_strptime(x))
    }
  }

  res
}
