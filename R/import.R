#' Import
#'
#' Import a single function from a package
#'
#' @param pkg String, name of the package
#' @param fun String, fun name of the function
#' @param overwrite Logical, if TRUE and `fun` is also found in the current
#'   environment, will overwrite assignment
#' @return None, called for side effects
#' @export
#'
#' @examples
#' # assigns `add` -- test with caution
#' import("magrittr", "add")

import <- function(pkg, fun, overwrite = FALSE) {
  e <- parent.frame()
  require_namespace(pkg)

  if (!overwrite && fun %in% ls(envir = e)) {
    stop(import_already_assigned(fun))
  }

  assign(fun, pkg %::% fun, envir = e)
}

# conditions --------------------------------------------------------------

import_already_assigned := condition(
  message = function(fun) sprintf("'%s' has already been assigned", fun),
  type = "error",
  exports = "import",
  help = "
The object you are trying to import has already been assigned in the environment you are importing to.  Use the `overwrite` option to replace the object.

For example:

```r
# instead of
foo <- NULL
import('package', 'foo')

# do this
foo <- NULL
import('package', 'foo', overwrite = TRUE)
```
")
