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

# caused by processing
value_error := condition(
  function(x) collapse(x),
  type = "error",
  help = "Generic error to indicate a value mismatch."
)

type_error := condition(
  function(expected, actual, object) {
    sprintf(
      ngettext(
        length(actual),
        "Expected %stype '%s' but got type '%s'",
        "Expected %stype '%s' but got types '%s'"
      ),
      if (missing(object)) "" else sprintf("object '%s' to be ", object),
      expected,
      collapse(actual, sep = "', '")
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
  function(type, x, must) {
    cls <- class(x)
    switch(
      type,
      not_supported = sprintf(
        ngettext(
          length(cls),
          "Class not supported: %s",
          "Classes not supported: %s",
        ),
        toString(cls)
      ),
      must_be = sprintf(
        ngettext(
          length(cls),
          "Object must be of class '%s', not '%s'",
          "Object must be of class '%s', not classes '%s'"
        ),
        toString(must),
        toString(cls)
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
