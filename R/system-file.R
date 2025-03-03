#' Make system file function
#'
#' Simple wrapper for package specific function for internal packages.
#'
#' @param package The name of the package
#'
#' @returns A `function` wrapping [system.file()] which will always use the
#' package name provided in `package`
#' @export
#' @examples
#' make_sf("mark")()
make_sf <- function(package) {
  fun <- function(..., check = FALSE) { }
  body(fun) <- substitute(
    match.fun("system.file")(..., package = package, mustWork = check)
  )
  fun
}

sf <- make_sf("mark")
