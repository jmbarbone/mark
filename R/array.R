#' Array extract
#'
#' Extract dimensions from an array
#'
#' @param arr An array
#' @param ... A named list by array dimension number and the value
#' @param default The default dimension index
#'
#' @export
#' @examples
#' x <- array(rep(NA, 27), dim = c(3, 3, 3))
#' x[1, 2, 3] <- TRUE
#' x[1, 2, 3]
#' array_extract(x, `2` = 2, `3` = 3)


array_extract <- function(arr, ..., default = "1") {
  if (!is.array(arr)) {
    stop("`arr` must be an array", call. = FALSE)
  }

  ls <- dotlist(...)

  nm <- as.integer(names(ls))

  if (anyNA(nm)) {
    stop("List must be named by numbers", call. = FALSE)
  }

  ds <- dim(arr)
  dn <- length(ds)
  setup <- rep(default, dn)

  for (i in seq_along(ls)) {
    val <- ls[[i]]

    if (is.character(val) || is.factor(val)) {
      val <- sprintf("'%s'", val)
    }

    setup[nm[i]] <- val
  }

  text <- sprintf(
    "%s[%s]",
    as.character(substitute(arr)),
    collapse0(setup, sep = ", ")
  )

  eval(parse(text = text), envir = parent.frame())
}

dotlist <- function(...) {
  if (tryCatch(is.list(...), error = function(e) FALSE)) {
    return(list(...)[[1]])
  }

  list(...)
}
