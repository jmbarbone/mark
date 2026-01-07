cnd_create_registry()

# nolint next: line_length_linter.
# https://github.com/RConsortium/S7/blob/e430c9aab137cc6301d5e06922e4d8cf2c0a8270/tests/testthat/helper.R#L73-L79
# modified from:
#   https://github.com/RConsortium/S7/issues/466#issuecomment-2417642612
walrus <- function(sym, val) {
  sym <- substitute(sym)
  sym <- as.character(sym) # `"sym" <- val` is fine

  val <- substitute(val)
  val <- as.list(val)
  val <- as.call(c(val[[1L]], sym, val[-1L]))

  eval.parent(call("<-", sym, val))
}

# https://adv-r.hadley.nz/quasiquotation.html
# := is like a vestigial organ: it’s recognised by R’s parser, but it doesn’t
# have any code associated with it. It looks like an = but allows expressions on
# either side, making it a more flexible alternative to =. It is used in
# data.table for similar reasons.
`:=` <- `%:=%` <- walrus


# standard conditions -----------------------------------------------------

# caused by user inputs
input_error := condition(
  function(x) collapse(x),
  type = "error",
  help = "Generic error to indicate a bad input value."
)

input_warning := condition(
  function(x) collapse(x),
  type = "warning",
  help = "Generic warning to indicate a bad input value."
)

# caused by processing
value_error := condition(
  function(x) collapse(x),
  type = "error",
  help = "Generic error to indicate a value mismatch."
)

value_warning := condition(
  function(x) collapse(x),
  type = "warning",
  help = "Generic warning to indicate a value mismatch."
)

type_error := condition(
  function(type, x, expected, actual, name) {
    if (missing(actual)) {
      actual <- typeof(x)
    }

    if (missing(name)) {
      name <- sprintf(
        "`%s`",
        deparse(
          match.call(
            sys.function(sys.parent(1L)),
            sys.call(sys.parent(1L)),
            envir = parent.frame(3L)
          )$x
        )
      )
    }

    switch(
      type,
      not_supported = sprintf(
        "Object `%s` cannot be of type: %s",
        name,
        actual
      ),
      must_be = sprintf(
        "Object `%s` must be of type '%s', not '%s'",
        name,
        expected,
        actual
      ),
      stop(internal_error())
    )
  },
  type = "error",
  help = "Generic error to indicate a type mismatch."
)

conversion_error := condition(
  function(x) collapse(x),
  type = "error",
  help = "Generic error to indicate a conversion failure."
)

class_error := condition(
  function(type, x, expected, actual, name) {
    if (missing(actual)) {
      actual <- class(x)
    }

    if (missing(name)) {
      name <- sprintf(
        "`%s`",
        deparse(
          match.call(
            sys.function(sys.parent(1L)),
            sys.call(sys.parent(1L)),
            envir = parent.frame(3L)
          )$x
        )
      )
    }

    switch(
      type,
      not_supported = sprintf(
        ngettext(
          length(actual),
          "Object `%s` cannot be of class: %s",
          "Object `%s` cannot be of classes: %s"
        ),
        name,
        toString(actual)
      ),
      must_be = sprintf(
        ngettext(
          length(actual),
          "Object `%s` must be of class '%s', not '%s'",
          "Object `%s` must be of class '%s', not classes '%s'"
        ),
        name,
        toString(expected),
        toString(actual)
      ),
      stop(internal_error())
    )
  },
  type = "error",
  help = "Generic error to indicate a class mismatch."
)

internal_error := condition(
  function(x = character()) {
    c(
      collapse(x),
      "\nThis is an internal `{mark}` error.  If you encounter this, please",
      " report an issue at <https://github.com/jmbarbone/mark/issues>"
    )
  },
  type = "error",
  help = c(
    "Generic error to capture internal `{mark}` errors.  If any of these are",
    " encountered, please report an issue at",
    " <https://github.com/jmbarbone/mark/issues>"
  )
)
