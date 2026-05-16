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

#' @importFrom cnd input_error input_warning value_error type_error class_error

internal_error := condition(
  function(x) {
    c(
      x,
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
