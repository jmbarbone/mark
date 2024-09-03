#' mapply with NULLs
#'
#' mapply (basically) but with NULLs
#'
#' @details
#' This works similarly to `mapply()` expect it can take zero-length variables
#'
#' @param FUN a function to call onto the parameters
#' @param params,... params to pass to `FUN`
#'
#' @examples
#' foo <- function(a, b, c) sum(a, b, c)
#' try(mapply(foo, a = 1:2, b = NULL, c = 3)) # produces error
#' mark:::mapply0(foo, a = 1:2, b = NULL, c = 3)
#'
#' # or pass as list
#' x <- list(a = 1:2, b = NULL, c = 3)
#' mark:::mapply0(foo, x)
#' @noRd
mapply0 <- function(FUN, params = NULL, ...) { # nolint: object_name_linter.
  params <- c(params,  rlang::list2(...))

  FUN <- match.fun(FUN) # nolint: object_name_linter.
  n <- max(lengths(params))

  p_list <- lapply(
    params,
    function(x)  {
      lx <- as.list(x)
      names(lx) <- x
      rep_len(lx, n)
    })

  result <- rep_len(list(), n)

  for (i in seq(n)) {
    result[[i]] <- do.call(FUN, lapply(p_list, `[[`, i))
  }

  result
}
