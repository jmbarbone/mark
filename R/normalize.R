#' Normalize values
#'
#' Normalizes values based on possible range and new bounds
#'
#' @param x An object that is (coercible to) `double`; `data.frames` are transformed
#' @param range The range of possible values of `x` as a vector of length `2`.
#'   Defaults to `range(x, na.rm = TRUE)`
#' @param bounds The new boundaries for the normalized value as a vector of
#'   length `2`.  Defaults to `c(0, 1)`.
#' @returns `x` with transformed values where `range` values are transformed to
#'   `bounds`.
#' @examples
#' x <- c(0.23, 0.32, 0.12, 0.61, 0.26, 0.24, 0.23, 0.32, 0.29, 0.27)
#' data.frame(
#'   x = normalize(x),
#'   v = normalize(x, range = c(0, 2)),
#'   b = normalize(x, bounds = c(0, 10)),
#'   vb = normalize(x, range = c(0, 2), bounds = c(0, 10))
#' )
#'
#' # maintains matrix
#' mat <- structure(c(0.24, 0.92, 0.05, 0.37, 0.19, 0.69, 0.43, 0.22, 0.85,
#' 0.73, 0.89, 0.68, 0.57, 0.89, 0.61, 0.98, 0.75, 0.37, 0.24, 0.24,
#' 0.34, 0.8, 0.25, 0.46, 0.03, 0.71, 0.79, 0.56, 0.83, 0.97), dim = c(10L, 3L))
#'
#' mat
#' normalize(mat, bounds = c(-1, 1))
#' normalize(as.data.frame(mat), bounds = c(-1, 1))
#' @export
normalize <- function(
    x,
    range = base::range(x, na.rm = TRUE),
    bounds = 0:1
) {
  UseMethod("normalize")
}

#' @rdname normalize
#' @export
normalize.default <- function(
    x,
    range = base::range(x, na.rm = TRUE),
    bounds = 0:1
) {
  x[] <- as.double(x)
  range <- base::range(range, na.rm = TRUE)
  bounds <- base::range(bounds, na.rm = TRUE)

  min <- range[1]
  max <- range[2]
  lower <- bounds[1]
  upper <- bounds[2]

  # these probably won't trigger unless there's a weird range method
  # nocov start
  stopifnot(
    length(range) == 2,
    length(bounds) == 2,
    min < max,
    lower < upper
  )
  # nocov end

  x[] <- lower + (x - min) * (upper - lower) / (max - min)
  x
}

#' @rdname normalize
#' @export
normalize.data.frame <- function(
    x,
    range = base::range(x, na.rm = TRUE),
    bounds = 0:1
) {
  x[] <- lapply(x, normalize, range = range, bounds = bounds)
  x
}