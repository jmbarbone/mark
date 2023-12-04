#' Write file with md5 hash check
#'
#' @param x An object to write to file
#' @param path The file or connection to write to (dependent on part by method)
#' @param method The method of saving the file.  When `default`, the method is
#'   determined by file extension of `path`, if present, otherwise by the type
#'   of object of `x`.
#' @inheritParams file_copy_md5
#' @param encoding The encoding to use when writing the file.
#' @param compression The compression method to use when writing the file.
#' @param ... Additional arguments passed to the write function.
#' @returns `x`, invisibly.  When `path` is not the `stdout()`, `x` is returned
#'   with the attribute `"path"` set to the result of [file_copy_md5()].
#' @examples
#' # just writes to stdout()
#' df <- data.frame(a = 1, b = 2)
#' write_file_md5(df)
#'
#' temp <- tempfile()
#' write_file_md5(df, temp) # new
#' write_file_md5(df, temp) # same
#' df$c <- 3
#' write_file_md5(df, temp) # changes
#' fs::file_delete(temp)
#' @export
write_file_md5 <- function(
    x,
    path = NULL,
    method = mark_write_methods(),
    overwrite = NA,
    quiet = FALSE,
    encoding = "UTF-8",
    compression = getOption("mark.compress.method", "default"),
    ...
) {
  op <- options(encoding = encoding, mark.compress.method = compression)
  on.exit(options(op), add = TRUE)

  if (!isTRUE(nzchar(path, keepNA = TRUE))) {
    null_path <- TRUE
    ext <- ""
  } else {
    path <- analyze_path(path)
    ext <- attr(path, "ext")
    null_path <- FALSE
  }

  method <- match_param(method, mark_write_methods())

  if (method == "default") {
    if (nzchar(ext)) {
      method <- ext
    } else if (is.data.frame(x)) {
      method <- "table"
    } else if (is.matrix(x)) {
      method <- "write"
    } else if (is.atomic(x)) {
      method <- "lines"
    } else {
      x <- as.list(x)
      method <- "json"
    }
  }

  write_function <- get(
    paste0("mark_write_", method),
    envir = asNamespace("mark"),
    mode = "function"
  )

  params <- rlang::list2(...)
  params$x <- x

  if (null_path) {
    params$con <- stdout()
  } else {
    temp <- fs::file_temp(ext = ext)
    on.exit(fs::file_delete(temp), add = TRUE)
    params$con <- compress(temp, compression)
    on.exit(safe_close(params$con), add = TRUE)
  }

  do.call(write_function, params)

  if (null_path) {
    return(invisible(x))
  }

  if (!is.null(attr(x, "path"))) {
    warning("attr(x, \"path\") is being overwritten")
  }

  attr(x, "path") <- file_copy_md5(
    path = temp,
    new_path = path,
    overwrite = overwrite,
    quiet = quiet
  )
  invisible(x)
}

mark_write_methods <- function() {
  list(
    "default",
    "csv",
    "csv2",
    "dcf",
    "json",
    lines = c("lines", "md", "txt", "qmd", "rmd"),
    "rds",
    table = c("table", "delim"),
    "tsv",
    "tsv2",
    "write",
    yaml = c("yaml", "yml")
  )
}

# write functions ---------------------------------------------------------

mark_write_write <- function(x, con, sep = " ") {
  write(x, file = con, ncolumns = NCOL(x), sep = sep)
}

mark_write_rds <- function(x, con, version = 3) {
  saveRDS(object = x, file = con, version = version)
}

mark_write_csv <- function(x, con, sep = ",", dec = ".", ...) {
  mark_write_table(x = x, con = con, sep = sep, dec = dec, ...)
}

mark_write_csv2 <- function(x, con, sep = ";", dec = ",", ...) {
  mark_write_table(x = x, con = con, sep = sep, dec = dec, ...)
}

mark_write_tsv <- function(x, con, sep = "\t", ...) {
  mark_write_table(x = x, con = con, sep = sep, ...)
}

mark_write_tsv2 <- function(x, con, sep = "|", ...) {
  mark_write_table(x = x, con = con, sep = sep, ...)
}

mark_write_table <- function(
    x,
    con = "",
    quote = TRUE,
    sep = " ",
    eol = "\n",
    na = "",
    dec = ".",
    # nolint next: object_name_linter.
    row.names = FALSE,
    # nolint next: object_name_linter.
    col.names = NA,
    qmethod = "escape"
) {
  if (isFALSE(row.names) && isNA(col.names)) {
    # nolint next: object_name_linter.
    col.names <- TRUE
  }

  utils::write.table(
    x = x,
    file = con,
    append = FALSE,
    quote = quote,
    sep = sep,
    eol = eol,
    na = na,
    dec = dec,
    row.names = row.names,
    col.names = col.names,
    qmethod = qmethod
  )
}

mark_write_dcf <- function(
    x,
    con = "",
    # nolint next: object_name_linter.
    useBytes = FALSE,
    indent = 4,
    width = Inf,
    # nolint next: object_name_linter.
    keep.white = NULL
) {
  write.dcf(
    x = x,
    file = con,
    append = FALSE,
    useBytes = useBytes,
    indent = indent,
    width = width,
    keep.white = keep.white
  )
}

mark_write_lines <- function(x, con, sep = "\n") {
  writeLines(text = x, con = con, sep = sep, useBytes = FALSE)
}

mark_write_yaml <- function(
    x,
    con,
    unicode = TRUE,
    digits = getOption("digits"),
    ordered_lists = TRUE
) {
  require_namespace("yaml")
  string <- yaml::as.yaml(
    x = x,
    line.sep = "\n",
    indent = 2L,
    column.major = TRUE,
    omap = ordered_lists,
    unicode = unicode,
    precision = digits,
    indent.mapping.sequence = FALSE,
    handlers = list(
      boolean = function(x) {
        if (x %in% c("n", "y")) {
          x
        } else {
          tolower(x) == "true"
        }
      }
    )
  )
  mark_write_lines(string, con)
}

mark_write_json <- function(x, con) {
  require_namespace("yaml")
  string <- jsonlite::toJSON(
    x,
    dataframe = "columns",
    matrix = "rowmajor",
    Date = "ISO8601",
    POSIXt = "ISO8601",
    factor = "string",
    complex = "string",
    raw = "base64",
    null = "null",
    na = "string",
    auto_unbox = FALSE,
    digits = getOption("digits"),
    pretty = TRUE,
    force = TRUE
  )
  mark_write_lines(string, con)
}

# helpers -----------------------------------------------------------------

compress <- function(
    x = "",
    method = getOption("mark.compress.method", "default"),
    encoding = getOption("mark.write_table.encoding", "UTF-8"),
    ...
) {
  op <- options(encoding = encoding)
  on.exit(options(op), add = TRUE)
  method <- match_param(method, c("default", "none", "gz", "bz2", "xz"))

  if (!identical(x, "")) {
    x <- fs::path(x)
    fs::dir_create(dirname(x))
  }

  if (method == "default") {
    method <- attr(analyze_path(x), "compress")
  }

  switch(
    method,
    none = file(x, ...),
    gz = gzfile(x, ...),
    bz2 = bzfile(x, ...),
    xz = xzfile(x, ...)
  )
}

analyze_path <- function(x) {
  ext <- tools::file_ext(x)

  if (ext %in% c("gz", "bz2", "xz")) {
    compress <- ext
    ext <- tools::file_ext(tools::file_path_sans_ext(x))
  } else {
    compress <- "none"
  }

  structure(
    fs::path(x),
    ext = ext,
    compress = compress
  )
}

safe_close <- function(con, ...) {
  tryCatch(
    close(con, ...),
    simpleError = function(e) {
      if (identical(conditionMessage(e), "invalid connection")) {
        return(invisible())
      }
      stop(e)
    }
  )
}
