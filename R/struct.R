#' Simple structures
#'
#' Create simple structures
#'
#' @details Unlike [base::structure()] this does not provide additional checks
#'   for special names, performs no [base::storage.mode()] conversions for
#'   `factors` (`x` therefor has to be an `integer`), `attributes` from `x` are
#'   not retained, and `class` is specified outside of other attributes and
#'   assigned after [base::attributes()] is called.
#'
#'   Essentially, this is just a wrapper for calling [base::attributes()] then
#'   [base::class()].
#'
#'   Note that [base::structure()] provides a warning when the first argument is
#'   `NULL`.  `struct()` does not.  The coercion from `NULL` to `list()` is
#'   done, and documented, in [base::attributes()].
#'
#' @param x An object; if `NULL`, coerced to `list()`
#' @param class A vector of classes; can also be `NULL`
#' @param ... Named attributes to set to `x`
#' @param .keep_attr Control for keeping attributes from `x`: `TRUE` will retain
#'   all attributes from `x`; a character vector will pick out specifically
#'   defined attributes to retain; otherwise only attributes defined in `...`
#'   will be used
#' @return An object with class defined as `class` and attributes `...`
#'
#' @export
#' @examples
#' x <- list(a = 1, b = 2)
#' # structure() retains the $names attribute of x but struct() does not
#' structure(x, class = "data.frame", row.names = 1L)
#' struct(x, "data.frame", row.names = 1L)
#' struct(x, "data.frame", row.names = 1L, names = names(x))
#'
#' # structure() corrects entries for "factor" class
#' # but struct() demands the data to be an integer
#' structure(1, class = "factor", levels = "a")
#' try(struct(1, "factor", levels = "a"))
#' struct(1L, "factor", levels = "a")
#'
#' # When first argument is NULL -- attributes() coerces
#' try(structure(NULL))    # NULL, no call to attributes()
#' struct(NULL, NULL)      # list(), without warning
#' x <- NULL
#' attributes(x) <- NULL
#' x                       # NULL
#' attributes(x) <- list() # struct() always grabs ... into a list
#' x                       # list()
#'
#' # Due to the use of class() to assign class, you may experience some
#' # other differences between structure() and struct()
#' x <- structure(1, class = "integer")
#' y <- struct(1, "integer")
#' str(x)
#' str(y)
#'
#' if (package_available("waldo")) {
#'   waldo::compare(x, y, x_arg = "structure", y_arg = "struct")
#' } else {
#'   all.equal(x, y)
#' }
#'
#' # Be careful about carrying over attributes
#' x <- quick_df(list(a = 1:2, b = 3:4))
#' struct(x, "data.frame", new = 1)
#' struct(x, "data.frame", new = 1, .keep_attr = TRUE)
#' struct(x, "data.frame", new = 1, .keep_attr = "names")
#' struct(x, "data.frame", new = 1, .keep_attr = c("names", "row.names"))

struct <- function(x, class, ..., .keep_attr = FALSE) {
  attributes(x) <- if (isTRUE(.keep_attr)) {
    c(attributes(x), list(...))
  } else if (is.character(.keep_attr)) {
    c(attributes(x)[.keep_attr], list(...))
  } else {
    list(...)
  }
  class(x) <- class
  x
}

