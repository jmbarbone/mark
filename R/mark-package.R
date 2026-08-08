#' @include not-available.R

#' mark
#'
#' Miscellaneous, Analytic R Kernels
#'
#' @importFrom fuj %||% %|||% %out% collapse list0
# nolint next: line_length_linter.
#' @importFrom cnd cnd condition cnd_create_registry cnd_exports cnd_document class_error duplicate_error input_error type_error value_error deprecated_warning duplicate_warning input_warning
#'
#' @docType package
#' @name mark
"_PACKAGE"


na_list <- list(
  logical = logical(),
  character = character(),
  integer = integer(),
  double = double(),
  numeric = numeric(),
  Date = as.Date(NA),
  POSIXct = as.POSIXct(NA),
  POSIXlt = as.POSIXlt(NA)[[1]]
)

# nolint next: object_name_linter.
op.mark <- list(
  mark.author = NULL,
  # control for check_interactive() to return interactive() or TRUE
  mark.check_interactive = TRUE,
  mark.note.width = NULL,
  mark.todos.ext = c("R", "Rmd", "qmd", "md", "py"),
  mark.todos.force = FALSE,
  mark.days_in_month = 30,
  mark.days_in_year = 365,
  mark.weeks_inn_year = 52,
  mark.default_tz = "UTC",
  mark.na_list = na_list,
  mark.md5.bytes = NULL,
  mark.tibble = TRUE
)

.onLoad <- function(libname, pkgname) {
  options(op.mark[!names(op.mark) %in% names(options())]) # nocov
}
