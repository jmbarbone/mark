#' Compute the MD5 hash of an object
#'
#' Wrapper for calling [tools::md5sum()] on objects rather than files.
#'
#' @details
#' When `x` is a `character` vector, [base::writeLines()] is used to write to
#' a temporary file.  Otherwise, an Rds file is created with [base::saveRDS()].
#' Coercing
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
  file <- file(path, encoding = "UTF-8")

  on.exit({
    safe_close(file)
    safe_fs_delete(path)
  })

  if (is.object(x)) {
    mark_write_rds(x, file)
  } else {
    x <- as.character(x)
    mark_write_lines(x, file)
  }

  struct(tools::md5sum(path), class = c("md5sum", "character"))
}

#' @export
print.md5sum <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}
