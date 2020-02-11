#' Alpha base
#'
#' Base 26 conversion with letters
#'
#' @param x A string of letters.  Non characters are removed.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' alpha_base("AB")
#' alpha_base("XFD")
#' alpha_base(c("JMB", "Jordan Mark", "XKCD"))

alpha_base <- function(x)  {
  vapply(x, function(x) {
    lets <- unlist(strsplit(tolower(x), ""), use.names = FALSE)
    lets <- lets[grepl("[[:alpha:]]", lets)]
    valid <- match(lets, letters, nomatch = NULL)
    if(anyNA(valid)) {
      stop("invalid character(s) found: ", paste(lets[is.na(valid)], collapse = ", "))
    }
    vals <- na.omit(valid)
    n <- length(vals)
    sum(c(vals[-n] * 26 ^ rev(seq(n-1)), vals[n]))
  },
  numeric(1), USE.NAMES = FALSE)
}

# base_n <- function(x, base) {
#   mapply(function(x, base) {
#     nums <- unlist(strsplit(x, ""), use.names = FALSE)
#     nums <- as.numeric(nums)
#     valid <- !is.na(nums)
#     if(anyNA(valid)) {
#       stop("invalid character(s) found: ", paste(nums[is.na(valid)], collapse = ", "))
#     }
#     vals <- na.omit(nums)
#     n <- length(vals)
#     sum(c(vals[-n] * base ^ rev(seq(n - 1)), vals[n]))
#   }, x, base, USE.NAMES = FALSE)
# }
#
# base_n("18", 5)
# base_n("25", 1:10)
