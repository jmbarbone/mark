#' Dataframe labels
#'
#' Assign labels to a vector or data.frame.
#'
#' @details When labels are assigned to a data.frame they can make viewing the
#' object (with `View()` inside Rstudio).  [mark::view_labels()] has a call to
#' `View()` inside and will retrieve the labels and show them in the viewer as a
#' data.frame.
#'
#' @param x A vector of data.frame
#' @param ... One or more unquoted expressed separated by commas.  If assigning
#'   to a data.frame, `...` can be replaced with a `data.frame` where the first
#'   column is the targeted column name and the second is the desired label.
#' @param label A single length string of a label to be assigned
#' @param cols A character vector of column names; if missing will remove the
#'   label attribute across all columns
#' @param title Title for the viewer window -- if not supplemented will show as
#'   `paste0(as.character(substitute(x)), " - Labels")`
#'
#' @return A labelled vector or `data.frame`
#' @name labels
#' @export
#'
#' @examples
#' labs <- assign_labels(
#'   iris,
#'   Sepal.Length = "cms",
#'   Sepal.Width  = "cms",
#'   Petal.Length = "cms",
#'   Petal.Width  = "cms",
#'   Species      = "Iris ..."
#' )
#'
#' labs$dummy <- ""
#' get_labels(labs) # shows label as <NA> for dummy column
#'
#' labs0 <- remove_labels(labs, c("Sepal.Length", "Sepal.Width"))
#' get_labels(labs0) # No labels for Sepal.Length and Sepal.Width

assign_labels <- function(x, ...) {
  UseMethod("assign_labels", x)
}

#' @export
#' @rdname labels
assign_labels.default <- function(x, label, ...) {
  if (length(label) != 1L) {
    stop(input_error("`label` must be a single length vector"))
  }
  attr(x, "label") <- label
  x
}

#' @export
#' @rdname labels
#' @param .missing A control setting for dealing missing columns in a list; can
#'   be set to `"error"` to [base::stop()] the call, `"warn"` to provide a
#'   warning, or `"skip"` to silently skip those labels.
#' @param .ls A named list of columns and labels to be set if `...` is empty
assign_labels.data.frame <- function(
  x,
  ...,
  .missing = c("error", "warn", "skip"),
  .ls = rlang::list2(...)
) {
  .missing <- match_param(.missing)

  # fmt: skip
  if (
    identical(.ls, list()) ||
    (...length() && !identical(rlang::list2(...), .ls)) ||
    is.null(.ls) ||
    any(vap_lgl(.ls, is.null))
  ) {
    stop(assign_labels_error("dots", dots = rlang::list2(...), ls = .ls))
  }

  if (inherits(.ls[[1L]], "data.frame")) {
    .ls <- struct(
      as.vector(.ls[[1L]][[2L]], "list"),
      class = "list",
      names = .ls[[1L]][[1L]]
    )
  }

  nm <- names(.ls)
  ma <- match(nm, colnames(x), nomatch = NA_integer_)

  if (anyNA(ma)) {
    nas <- is.na(ma)

    switch(
      .missing,
      error = stop,
      warn = warning,
      skip = function(cond) NULL
    )(
      assign_labels_error("columns", cols = nm[nas])
    )

    nm <- nm[!nas]
    ma <- ma[!nas]
    .ls <- .ls[!nas]
  }

  for (i in seq_along(nm)) {
    mi <- ma[i]
    x[[mi]] <- assign_labels(x[[mi]], .ls[[i]])
  }

  x
}

#' @export
#' @rdname labels
get_labels <- function(x) {
  UseMethod("get_labels", x)
}

#' @export
#' @rdname labels
get_labels.default <- function(x) {
  exattr(x, "label") %||% NA_character_
}

#' @export
#' @rdname labels
get_labels.data.frame <- function(x) {
  vector2df(vap_chr(x, get_labels, .nm = TRUE), "column", "label")
}

#' @export
#' @rdname labels
view_labels <- function(x, title) {
  cesx <- charexpr(substitute(x))

  if (missing(title)) {
    title <- paste0(cesx, " - Labels")
  }

  if (!interactive()) {
    view <- function(x, title) print(x)
  } else if ("tools:rstudio" %in% search()) {
    view <- get("View", as.environment("tools:rstudio"))
  } else {
    view <- utils::View
  }

  tryCatch(
    view(x = get_labels(x), title = title),
    error = function(cond) {
      stop(view_labels_error())
    }
  )
}

#' @export
#' @rdname labels
remove_labels <- function(x, ...) {
  UseMethod("remove_labels", x)
}

#' @export
#' @rdname labels
remove_labels.default <- function(x, ...) {
  attr(x, "label") <- NULL
  x
}

#' @export
#' @rdname labels
remove_labels.data.frame <- function(x, cols, ...) {
  if (missing(cols)) {
    cols <- seq_along(x)
  } else {
    bad <- cols %wo% colnames(x)

    if (length(bad)) {
      stop(assign_labels_error(bad))
    }
  }

  for (i in cols) {
    x[[i]] <- remove_labels(x[, i])
  }

  x
}

# conditions --------------------------------------------------------------

assign_labels_error := condition(
  function(s, ..., cols, dots, ls) {
    switch(
      s,
      columns = sprintf(
        ngettext(
          length(cols),
          "Columns not found: %s",
          "Column not found: %s"
        ),
        toString(cols)
      ),
      dots = if (identical(dots, ls)) {
        sprintf("labels provided are malformed: %s", toString(dots))
      } else {
        sprintf(
          "`.ls` and `...` were both set and/or malformed\n ... %s\n .ls %s",
          toString(dots),
          toString(ls)
        )
      },
      stop(internal_error())
    )
  },
  type = "error",
  exports = c("assign_labels", "remove_labels"),
  # nolint start: line_length_linter.
  help = r"(
`assign_labels_error`

**Columns not found**
You can set `.missing` to `warn` to get a warning instead of an error, or `skip` to silently skip those labels.

**Malformed labels**
If passing labels as `.ls`, `...` must be empty. Columns in `...` must be entered as `name = value`; `column = label`.

```r
# instead of this:
assign_labels(df, a = 'AAA', .ls = list(b = 'BBB'))

# do this:
assign_labels(df, a = 'AAA', b = 'BBB')
# or this:
assign_labels(df, .ls = list(a = 'AAA', b = 'BBB'))
```
Labels must not be null; to remove lavels, use `remove_labels()`.

```r
# instead of this
assign_labels(df, a = 'AAA', b = NULL)

# do this
df <- assign_labels(df, a = 'AAA')
df <- remove_labels(df, 'b')
```)"
)
# nolint end: line_length_linter.

view_labels_error := condition(
  "Cannot use `View()`",
  type = "error",
  exports = "view_labels",
  help = c(
    "This may be because you are using RStudio :",
    "  https://community.rstudio.com/t/view-is-causing-an-error/75297/4 ",
    "You can try :",
    '  `utils::View(get_labels(x), title = "Labels")`'
  )
)

remove_labels_error := condition(
  function(x) {
    sprintf(
      ngettext(
        length(x),
        "Column not found in data.frame: %s",
        "Columns not found in data.frame: %s"
      ),
      toString(x)
    )
  },
  type = "error",
  exports = "remove_labels"
)
