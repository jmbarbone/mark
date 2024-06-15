#' Compute the MD5 hash of an object
#'
#' Wrapper for calling [tools::md5sum()] on objects rather than files.
#'
#' @details All `x` objects are [serialized][base::serialize()] to a temporary
#' file before [tools::md5sum()] is called.
#'
#' @param x An object
#' @return A `md5sum` object
#' @export
#' @examples
#' md5("hello")
#' md5(1:10)
#' md5(data.frame(a = 1:10, b = letters[1:10]))
md5 <- function(x) {
  path <- fs::file_temp("mark_md5__")
  on.exit(safe_fs_delete(path), add = TRUE)

  con <- file(path, "wb", encoding = "UTF-8", raw = TRUE)
  on.exit(safe_close(con), add = TRUE)

  # nolint next: line_length_linter.
  # https://github.com/yihui/xfun/blob/6de6590306e7493dec8c457b5baa0b371735ad0d/R/cache.R#L396-L408
  s <- serialize(x, NULL, ascii = FALSE, version = 3L)
  writeBin(s[seq.int(15L, length(s))], con, useBytes = TRUE)
  close(con)
  struct(tools::md5sum(path), class = c("md5sum", "character"))
}

#' @export
print.md5sum <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}
