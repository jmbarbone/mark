#' Append a note to an object
#'
#' An alternative to the [base::comment()].
#'
#' @details
#' When the note is assigned to an object a new class will be added,
#'   `note`, so that a `print` function can call an S3 method.  The print for
#'    this can be adjusted for it's width by using the option
#'   `mark.note.width` which defaults to the option `width` when not set.
#'
#' The type of object assigned to the note is not restricted, so user beware
#'   of odd prints or additional features added to the notes fun.
#'
#' When assigning a note (with `note<-`) the `noted` class is added to the
#'   object.  This allows the `print.noted` class to be distracted and for the
#'   note to be printed (with the `print.note` method) every time the object is
#'   called/printed.  However, it will not be called when not `interactive()`
#'
#' @param x An object
#' @param value The note to attach; if `NULL` will remove the note and the
#'   class `noted` from the object.
#' @param ... Additional arguments passed from methods (not used)
#' @return
#' * `note<-` will return `x` (with the `"note"` attribute assigned)
#' * `note()` will retrieve the `"note"` attribute
#'
#' @examples
#' x <- c("x", "k", "c", "d")
#' comment(x) <- "This is just a comment"
#' comment(x)
#'
#' # Comment is intentionally hidden
#' x
#' note(x) <- "Just some random letters"
#' note(x)
#'
#' # Note is now present every time
#' x
#'
#' # Assigning `NULL` will remove note (and class)
#' note(x) <- NULL
#' note(x) # NULL
#' x       # No more note
#'
#' @name note
#' @export

`note<-` <- function(x, value) {
  if (is.null(value)) {
    attr(x, "note") <- NULL
    class(x) <- setdiff(class(x), "noted")
    return(x)
  }

  # TODO test that class "note" or "noted" is not continuously appended
  if (!inherits(value, "note")) {
    class(value) <- c("note", class(value))
  }

  if (!inherits(x, "noted")) {
    class(x) <- c("noted", class(x))
  }

  attr(x, "note") <- value
  x
}

#' @export
#' @rdname note
note <- function(x) {
  attr(x, "note")
}

#' @exportS3Method
#' @rdname note
print.note <- function(x, ...) {
  if (!interactive()) {
    return(invisible(x))
  }
  width <- getOption("mark.note.width", getOption("width"))
  out <- vap_chr(paste0("Note :  ", x), str_slice_by_word, width)
  cat(crayon_blue(out), sep = "\n")
  invisible(x)
}

#' @exportS3Method
print.noted <- function(x, ...) {
  n <- attr(x, "note")
  y <- x
  class(y) <- setdiff(class(x), "noted")
  attr(y, "note") <- NULL
  print(n)
  print(y)
  invisible(x)
}
