#' Compute the MD5 hash of an object
#'
#' Wrapper for calling [tools::md5sum()] on objects rather than files.
#'
#' @details All `x` objects are [serialized][base::serialize()] to a temporary
#'   file before [tools::md5sum()] is called.
#'
#' @param x An object
#' @param bytes If `TRUE` will use the `bytes` argument in [tools::md5sum()],
#'   available in R >= 4.5.0. If `NA` (the default) will use `TRUE` if
#'   available.
#' @return A `md5sum` object
#' @export
#' @examples
#' md5("hello")
#' md5(1:10)
#' md5(data.frame(a = 1:10, b = letters[1:10]))
md5 <- function(x, bytes = getOption("mark.md5.bytes")) {
  # xdr: `FALSE` uses little endian, which is more portable between machines and
  # faster
  s <- serialize(x, NULL, xdr = FALSE)[-(1:14)]

  bytes_available <- getRversion() >= "4.5.0"

  if (is.null(bytes)) {
    bytes <- bytes_available
  } else if (bytes && !bytes_available) {
    # nocov start
    warning(
      "`mark::file_copy_md5(bytes = TRUE)` is only available on R versions >=",
      " 4.5.0",
      call. = FALSE
    )
    bytes <- FALSE
    # nocov end
  }

  if (bytes) {
    return(struct(tools::md5sum(bytes = s), class = c("md5sum", "character")))
  }

  path <- fs::file_temp("mark_md5__")
  on.exit(safe_fs_delete(path), add = TRUE)

  con <- file(path, "wb", encoding = "UTF-8", raw = TRUE)
  on.exit(safe_close(con), add = TRUE)

  # nolint next: line_length_linter.
  # https://github.com/yihui/xfun/blob/6de6590306e7493dec8c457b5baa0b371735ad0d/R/cache.R#L396-L408
  writeBin(s, con, useBytes = TRUE)
  close(con)
  struct(tools::md5sum(path), class = c("md5sum", "character"))
}

#' @export
print.md5sum <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}
