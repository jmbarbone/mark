#' Write file with md5 hash check
#'
#' @section `options()`:
#'
#' - `mark.compress.method`: compression method to use when writing files
#' - `mark.list.hook`: when a `data.frame` contains a `list` column, this
#'   function is applied to each element of the list.  The default `"auto"`
#'   uses `toJSON()` if the package `jsonlite` is available, otherwise
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
#' @returns
#' - [write_file_md5()]: `x`, invisibly.  When `path` is not the `stdout()`, `x`
#' is returned with the attribute `"path"` set to the result of
#' [file_copy_md5()].
#' - [mark_write_methods()]: A list of applicable methods and their aliases
#' - [mark_compress_methods()]: A character vector of applicable compression
#' methods
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
    compression = getOption("mark.compress.method", mark_compress_methods()),
    ...
) {
  compression <- match_param(compression, mark_compress_methods())
  op <- options(
    encoding = encoding,
    mark.compress.method = compression,
    mark..compress.method = NULL
  )
  on.exit(options(op), add = TRUE)

  if (
    !isTRUE(nzchar(path, keepNA = TRUE)) ||
    inherits(path, "terminal")
  )  {
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
      method <- match_param(ext, mark_write_methods())
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
    params$con <- path %||% stdout()
  } else {
    if (compression == "default") {
      compression <- attr(path, "compress")
    }

    if (compression != "none") {
      ext <- paste0(ext, ".", compression)
    }

    temp <- fs::file_temp(ext = ext)
    attributes(temp) <- attributes(path)
    on.exit(safe_fs_delete(temp), add = TRUE)
    params$con <- compress(temp, compression)
    on.exit(safe_close(params$con), add = TRUE)
  }

  do.call(write_function, params)

  if (null_path) {
    return(invisible(x))
  }

  hook <- attr(params$con, "hook") %||% "invisible"
  hook <- match.fun(hook)
  temp <- hook(temp) %||% temp

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

#' @export
#' @rdname write_file_md5
mark_write_methods <- function() {
  list(
    "default",
    "csv",
    "csv2",
    "csv3",
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

#' @export
#' @rdname write_file_md5
mark_compress_methods <- function() {
  c("default", "none", "gz", "bz2", "xz", "zip", "tar")
}

# write functions ---------------------------------------------------------

mark_write_write <- function(x, con, sep = " ") {
  write(x, file = con, ncolumns = NCOL(x), sep = sep)
}

mark_write_rds <- function(x, con, version = 3) {
  saveRDS(object = x, file = con, version = version)
}

mark_write_csv <- function(
    x,
    con,
    sep = ",",
    dec = ".",
    qmethod = "double",
    ...
) {
  mark_write_table(
    x = x,
    con = con,
    sep = sep,
    dec = dec,
    qmethod = qmethod,
    ...
  )
}

mark_write_csv2 <- function(
    x,
    con,
    sep = ";",
    dec = ",",
    qmethod = "double",
    ...
) {
  mark_write_table(
    x = x,
    con = con,
    sep = sep,
    dec = dec,
    qmethod = qmethod,
    ...
  )
}

mark_write_csv3 <- function(
    x,
    con,
    sep = "|",
    dec = ".",
    qmethod = "double",
    ...
) {
  mark_write_table(
    x = x,
    con = con,
    sep = sep,
    dec = dec,
    qmethod = qmethod,
    ...
  )
}

mark_write_tsv <- function(x, con, sep = "\t", qmethod = "double", ...) {
  mark_write_table(x = x, con = con, sep = sep, qmethod = qmethod, ...)
}

mark_write_tsv2 <- function(x, con, sep = "|", qmethod = "double", ...) {
  mark_write_table(x = x, con = con, sep = sep, qmethod = qmethod, ...)
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
    qmethod = "escape",
    list_hook = getOption("mark.list.hook", "auto")
) {
  if (isFALSE(row.names) && isNA(col.names)) {
    # nolint next: object_name_linter.
    col.names <- TRUE
  }

  if (identical(list_hook, "auto") || isTRUE(list_hook)) {
    if (package_available("jsonlite")) {
      list_hook <- "mark_to_json"
    } else {
      list_hook <- function(x) collapse(shQuote(x, "sh"), sep = ",")
    }
  } else if (isFALSE(list_hook)) {
    list_hook <- function(x) NA_character_
  } else if (isNA(list_hook)) {
    list_hook <- function(x) {
      stop(new_condition(
        "options(mark.list.hook) is NA but list columns detected",
        class = "writeFileMd5ListHook"
      ))
    }
  }

  if (!isFALSE(list_hook) && !is.null(list_hook)) {
    list_hook <- match.fun(list_hook)
    ok <- vap_lgl(x, is.list)
    if (any(ok)) {
      x[ok] <- lapply(x[ok], function(i) vap_chr(i, list_hook))
    }
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
        # nocov start
        if (x %in% c("n", "y")) {
          x
        } else {
          tolower(x) == "true"
        }
        # nocov end
      }
    )
  )
  mark_write_lines(string, con)
}

mark_write_json <- function(x, con) {
  require_namespace("yaml")
  string <- mark_to_json(x)
  mark_write_lines(string, con)
}

# helpers -----------------------------------------------------------------

mark_to_json <- function(x) {
  jsonlite::toJSON(
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
}

compress <- function(
    x = "",
    method = getOption("mark.compress.method", "default"),
    encoding = getOption("mark.write_table.encoding", "UTF-8"),
    ...
) {
  op <- options(encoding = encoding)
  on.exit(options(op), add = TRUE)
  method <- match_param(method, mark_compress_methods())

  if (!identical(x, "")) {
    fs::dir_create(dirname(x))
  }

  path <- analyze_path(x)

  if (method == "default") {
    method <- attr(path, "compress")
  }

  if (identical(attr(path, "extra"), "tar")) {
    if (method == "zip") {
      stop(
        "'zip' is not a valid method when writing to a tar archive",
        call. = FALSE
      )
    }
    # tar() can do the compression for us, so we move this to a temporary
    # option.  this is always cleaned up because it shouldn't be set manually
    options(mark..compress.method = method)
    method <- "tar"
  }

  switch(
    method,
    none = file(x, ...),
    gz = gzfile(x, ...),
    bz2 = bzfile(x, ...),
    xz = xzfile(x, ...),
    zip = structure(
      file(x, ...),
      hook = function(x) {
        on.exit(safe_fs_delete(new), add = TRUE)
        # file path coming in as the .zip file, but we want to move this over,
        # and then save the .zip file to the new location
        new <- tools::file_path_sans_ext(x)
        fs::file_move(x, new)

        params <- list(...)
        params$zipfile <- x
        params$files <- new
        params$flags <- params$flags %||% "-q"
        do.call(utils::zip, params)
        invisible()
      }
    ),
    tar = structure(
      file(x, ...),
      hook = function(x) {
        on.exit(safe_fs_delete(x), add = TRUE)
        params <- list(...)
        params$tarfile <- x
        fs::path_ext(params$tarfile) <- "tar"
        params$files <- x
        # applies the compression method to the tar file
        params$compression <-
          params$compression %||% getOption("mark..compress.method")

        if (params$compression != "default") {
          params$tarfile <- paste0(params$tarfile, ".", params$compression)
        }

        do.call(utils::tar, params)
        params$tarfile
      }
    )
  )
}

analyze_path <- function(x) {
  if (isTRUE(attr(x, "analyzed"))) {
    return(x)
  }

  ext <- tools::file_ext(x)
  extra <- NULL

  if (ext %in% mark_compress_methods()[-1]) {
    compress <- ext
    ext <- tools::file_ext(tools::file_path_sans_ext(x))
    if (identical(ext, "tar")) {
      extra <- "tar"
      ext <- tools::file_ext(
        tools::file_path_sans_ext(tools::file_path_sans_ext(x))
      )
    }
  } else {
    compress <- "none"
  }

  structure(
    fs::path(x),
    ext = ext,
    compress = compress,
    analyzed = TRUE,
    extra = extra
  )
}

safe_close <- function(con, ...) {
  tryCatch(
    close(con, ...),
    simpleError = function(e) {
      if (identical(conditionMessage(e), "invalid connection")) {
        return(invisible())
      }
      stop(e) # nocov
    }
  )
}

safe_fs_delete <- function(x) {
  if (fs::file_exists(x)) {
    fs::file_delete(x)
  }
}
