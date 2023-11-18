#' Write file with md5 hash check
#'
#' @param data A data object to write to file
#' @param file The file or connection to write to (dependent on part by method)
#' @param method The method of saving the file.  When `NULL`, the method is
#'   determined by the file extension.
#' @inheritParams file_copy_ms
#' @param ... Additional arguments passed to the write function.
#' @returns See [file_copy_ms()].  When `file` is a connection, the returns
#'   nothing and writes the file to the `stdout()`
#' @examples
#' df <- data.frame(a = 1, b = 2)
#'
#' # just writes to stdout()
#' write_file_ms(df)
#'
#' temp <- tempfile()
#' # new
#' write_file_ms(df, temp)
#'
#' # same
#' write_file_ms(df, temp)
#'
#' # changed
#' df$c <- 3
#' write_file_ms(df, temp)
#'
#' fs::file_delete(temp)
#' @export
write_file_ms <- function(
    data,
    path = NULL,
    method = NULL,
    overwrite = NA,
    quiet = FALSE,
    ...
) {
  if (is.null(path)) {
    null_path <- TRUE
    ext <- ""
  } else {
    null_path <- FALSE
    ext <- fs::path_ext(path)
  }

  if (null_path || ext == "") {
    if (is.data.frame(data)) {
      method <- "table"
    } else {
      if (!is.atomic(data)) {
        warning(
          "don't know what to do with class ", toString(class(data)),
          "\ntrying to convert to character",
          call. = FALSE
        )
        data <- as.character(data)
      }
      method <- "lines"
    }
  }

  method <- mark::match_param(method, list(
    "csv",
    "csv2",
    "dcf",
    lines = c("lines", "md", "txt", "qmd", "rmd"),
    "rds",
    table = c("table", "delim"),
    "tsv",
    "tsv2"
  ))

  write_function <- switch(
    method,
    csv = function(x, ...) utils::write.csv(x = x, ...),
    csv2 = function(x, ...) utils::write.csv2(x = x, ...),
    dcf = function(x, ...) write.dcf(x = x, ...),
    lines = function(x, ...) writeLines(text = x, con = path, ...),
    rds = function(x, ...) saveRDS(object = x, ...),
    table = function(x, ...) utils::write.table(x = x, ...),
    tsv = function(x, ...) utils::write.table(x = x, sep = "\t", ...),
    tsv2 = function(x, ...) utils::write.table(x = x, sep = "|", ...),
    NULL
  )

  params <- rlang::list2(...)
  params$x <- data
  params$file <- if (null_path) {
    stdout()
  } else {
    fs::file_temp(ext = ext)
  }

  if (method %in% c("dcf", "table", "tsv", "tsv2")) {
    if (!is.null(params$append) && !isFALSE(params$append)) {
      stop(
        "method '", method,
        "' only supports `append = FALSE` in `...`",
        " All other values are ignored.",
        call. = FALSE)
    }
    params$append <- FALSE
  }

  do.call(write_function, params)

  if (null_path) {
    return(invisible())
  }

  file_copy_ms(params$file, path, overwrite = overwrite, quiet = quiet)
}
