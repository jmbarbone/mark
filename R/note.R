#' Append a note to an object
#'
#' An alternative to the [base::comment()].
#'
#' @details When the note is assigned to an object a new class will be added,
#'   `note`, so that a `print` function can call an S3 method.  The print for
#'   this can be adjusted for it's width by using the option `mark.note.width`
#'   which defaults to the option `width` when not set.
#'
#'   The type of object assigned to the note is not restricted, so user beware
#'   of odd prints or additional features added to the notes fun.
#'
#'   When assigning a note (with `note<-`, and its alias `set_note()`) the
#'   `noted` class is added to the object.  This allows the `print.noted` class
#'   to be dispatched and for the note to be printed every time the object is
#'   called/printed and the next print method used.  However, it will not be
#'   called when not `interactive()`
#'
#' @param x An object
#' @param value The note to attach; if `NULL` will remove the note and the class
#'   `noted` from the object.
#' @return
#' * `note<-`, `set_note()` will return `x` (with the `"note"` attribute
#'    assigned)
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
    x <- remove_attributes(x, "note")
    x <- remove_class(x, "noted")
    return(x)
  }

  if (!inherits(value, "note")) {
    value <- add_class(value, "note", pos = 1L)
  }

  if (!inherits(x, "noted")) {
    x <- add_class(x, "noted", pos = 1L)
  }

  add_attributes(x, note = value)
  attr(x, "note") <- value
  x
}

#' @export
#' @rdname note
set_note <- `note<-`

#' @export
#' @rdname note
note <- function(x) {
  attr(x, "note")
}

#' @export
print.note <- function(x, ...) {
  print(remove_class(x, "note"))
  invisible(x)
}

print_note <- function(x, ...) {
  stopifnot(inherits(x, "noted"))

  the_note <- note(x)

  if (!inherits(the_note, "note")) {
    stop(cond_print_note_note())
  }

  if (!check_interactive()) {
    return(invisible(x))
  }

  width <- getOption("mark.note.width", getOption("width"))
  out <- vap_chr(paste0("Note : ", the_note), str_slice_by_word, width)
  cat(crayon_blue(out), sep = "\n")
  invisible(x)
}

#' @export
print.noted <- function(x, ...) {
  print_note(x)
  print(set_note(x, NULL), ...)
  invisible(x)
}

# conditions --------------------------------------------------------------

cond_print_note_note <- function() {
  new_condition("note(x) must be class note", "print_note_noted")
}
