#' Assign add
#'
#' this needs to be tested...
#'
#' @param e1 Object to be updated
#' @param e2 right side statement
#'
#' @export
#'
#' @examples
#' val <- 1
#' val %=+% 2
#' val # 3
#' val %=+% 2
#' val # 5
#' val %=-% 1
#' val # 4
#' \dontrun{
#' val %=+% c(1, 2, 3) # fails
#' }
#' val <- c(1, 2, 3)
#' val %=+% c(1, 2, 3) # 2 4 6
#' @name assigns
#' @export

`%=+%` <- function(e1, e2) {
  if (length(e1) != length(e2)) {
    stop("Lengths of e1 and e2 are not the same", call. = FALSE)
  }

  res <- e1 + e2
  assign(
    x = as.character(substitute(e1)),
    value = res,
    envir = parent.frame(),
    inherits = FALSE
  )
  invisible(res)
}

#' @rdname assigns
#' @export
`%=-%` <- function(e1, e2) {
  if (length(e1) != length(e2)) {
    stop("Lengths of e1 and e2 are not the same", call. = FALSE)
  }

  res <- e1 - e2
  assign(
    x = as.character(substitute(e1)),
    value = res,
    envir = parent.frame(),
    inherits = FALSE
  )
  invisible(res)
}
