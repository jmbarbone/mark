
#' File copy with md5 hash check
#'
#' @inheritParams fs::file_copy
#' @param overwrite When `NA`, only saves if the md5 hashes do not match.
#'   Otherwise, see [fs::file_copy()].
#' @param quiet When `TRUE`, suppresses messages from md5 checks.
#' @export
#' @returns The path(s) of the new file(s), invisibly.  When `overwrite` is
#'   `NA`, the paths will be returned with two addition attributes, `"changed"`,
#'   a logical vector indicating whether the file was changed (`NA` for when the
#'   file is new), and `"md5sum"`, a list of the md5sums of the old and new md5
#'   sums.
file_copy_md5 <- function(path, new_path, overwrite = NA, quiet = FALSE) {
  msg <- if (quiet) {
    function(...) invisible()
  } else if (utils::packageVersion("fuj") < "0.2.2") {
    function(...) message(...)
  } else {
    function(...) message(cond_file_copy_md5(...))
  }

  # not as pretty, but pretty reasonable
  stopifnot(length(path) == length(new_path))
  # md5sum(nonexisting_file) produces NA
  md_old <- unname(tools::md5sum(path))
  md_new <- unname(tools::md5sum(new_path))
  changed <- md_old != md_new
  ok <- changed | is.na(changed)

  if (any(ok, na.rm = TRUE)) {
    # fs::file_copy(character(), character()) currently works, but I want to be
    # safe
    fs::file_copy(path[ok], new_path[ok], overwrite = TRUE)
  }

  msg({
    code <- match(changed, c(FALSE, TRUE, NA))
    out <- new_path
    out[code == 1L] <- paste(new_path[code == 1L], "(md5 same)")
    out[code == 2L] <- paste(new_path[code == 2L], "(md5 change)")
    out[code == 3L] <- paste(new_path[code == 3L], "(new file)")
    fuj::collapse(out, sep = "\n")
  })

  attr(new_path, "changed") <- changed
  attr(new_path, "md5sum") <- list(old = md_old, new = md_new)
  invisible(new_path)
}

cond_file_copy_md5 <- function(...) {
  fuj::new_condition(
    .makeMessage(...),
    class = "file_copy_md5",
    type = "message"
  )
}
