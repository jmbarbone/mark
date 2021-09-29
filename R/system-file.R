#' Make system file function
#'
#' Simple wrapper for package specific function for internal packages
#'
#' @param package The name of the package
#' @export

make_sf <- function(package) {
  function(..., check = FALSE) {
    match.fun("system.file")(..., package = package, mustWork = check)
  }
}

sf <- make_sf("mark")
