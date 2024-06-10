#' Compute the MD5 hash of an object
#' @param x An object
#' @return A single `character`
#' @export
#' @examples
#' md5("hello")
#' md5(1:10)
#' md5(data.frame(a = 1:10, b = letters[1:10]))
md5 <- function(x) {
  UseMethod("md5")
}

#' @rdname md5
#' @export
md5.character <- function(x) {
  do_md5(x, writeLines)
}

#' @rdname md5
#' @export
md5.default <- function(x) {
  do_md5(x, mark_write_rds)
}

do_md5 <- function(x, fun) {
  file <- tempfile("mark_md5__")
  on.exit(file.remove(file))
  fun(x, file)
  unname(tools::md5sum(file))
}