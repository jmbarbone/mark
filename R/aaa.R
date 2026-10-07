cnd_create_registry()

# nolint next: line_length_linter.
# https://github.com/RConsortium/S7/blob/e430c9aab137cc6301d5e06922e4d8cf2c0a8270/tests/testthat/helper.R#L73-L79
# modified from:
#   https://github.com/RConsortium/S7/issues/466#issuecomment-2417642612
walrus <- function(sym, val) {
  sc <- sys.call()
  sc[[1L]] <- quote(`<-`)
  sc[[3L]]$name <- as.character(sc[[2L]])
  Exec(sc, parent.frame())
}

# https://adv-r.hadley.nz/quasiquotation.html
# := is like a vestigial organ: it’s recognised by R’s parser, but it doesn’t
# have any code associated with it. It looks like an = but allows expressions on
# either side, making it a more flexible alternative to =. It is used in
# data.table for similar reasons.
`%:=%` <- walrus # NOTE not yet using `:=` directly


# standard conditions -----------------------------------------------------

internal_error %:=%
  condition(
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
