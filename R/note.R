#' Append a note to an object
#'
#' An alternative to the [base::comment()].
#'
#' @details
#' When the note is assigned to an object a new class will be added,
#'   `jordan_note`, so that a `print` function can call an S3 method.  The print
#'   for this can be adjusted for it's width by using the option
#'   `jordan.note.width` which defaults to the option `width` when not set.
#'
#' The method of _printing_ can also be adjusted with the `jordan.note.fun`
#'   option in which a function can be assigned.  This function will then be
#'   applied to the note at print.
#'
#' The type of object assigned to the note is not restricted, so user beware
#'   of odd prints or additional features added to the notes fun.
#'
#' @param x An object
#' @param value The note to attach
#' @param ... Additional arguments passed from methods (not used)
#'
#' @importFrom methods getFunction
#'
#' @export
#' @name note

"note<-" <- function(x, value) {
  if (!is.null(value)) {
    cls <- append("jordan_note", class(value))
    class(value) <- cls
  }
  attr(x, "jordan_note") <- value
  x
}

#' @export
#' @rdname note
note <- function(x) {
  a <- attr(x, "jordan_note")

  if (is.null(a)) {
    return(NULL)
  }

  if (!inherits(a, "jordan_note")) {
    class(a) <- append("jordan_note", class(a))
  }

  print(a)
}

#' @export
#' @rdname note
print.jordan_note <- function(x, ...) {
  width <- getOption("jordan.note.width", getOption("width"))
  out <- lapply(x, str_slice_by_word, width)
  out <- paste(unlist(out), collapse = "\n")
  fun <- getOption("jordan.note.fun", message)

  if (!is.function(fun)) {
    fun <- tryCatch(
      getFunction(fun),
      error = function(e) {
        stop(e, "Review option `jordan.note.fun`", call. = FALSE)
      })
  }

  fun(out)
  invisible(x)
}
