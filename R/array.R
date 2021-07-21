#' Array extract
#'
#' Extract dimensions from an array
#'
#' @param .arr An array
#' @param ... A named list by array dimension number and the value
#' @param default The default dimension index
#'
#' @return A value from the array `arr`
#' @export
#' @examples
#' x <- array(rep(NA, 27), dim = c(3, 3, 3))
#' x[1, 2, 3] <- TRUE
#' x[1, 2, 3]
#' x
#' array_extract(x, `2` = 2, `3` = 3)

array_extract <- function(.arr, ..., default = "1") {
  ls <- dotlist(...)

  if (!is.array(.arr)) {
    stop(".arr must be an array", call. = FALSE)
  }

  nm <- wuffle(as.integer(names(ls) %||% seq_along(ls)))

  if (anyNA(nm)) {
    stop("... must be fully named by integers or have no names", call. = FALSE)
  }

  ds <- dim(.arr)
  dn <- length(ds)
  setup <- rep(default, dn)

  for (i in seq_along(ls)) {
    val <- ls[[i]]

    if (is.factor(val)) {
      val <- as.character(val)
    }

    if (is.character(val)) {
      val <- sprintf("'%s'", val)
    }

    setup[nm[i]] <- val
  }

  text <- sprintf(
    "%s[%s]",
    as.character(substitute(.arr)),
    collapse0(setup, sep = ", ")
  )

  eval(str2expression(text), envir = parent.frame())
}

dotlist <- function(...) {
  if (tryCatch(is.list(...), error = function(e) FALSE)) {
    return(list(...)[[1]])
  }

  list(...)
}
