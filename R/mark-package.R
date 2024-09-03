#' mark
#'
#' Miscellaneous, Analytic R Kernels
#'
#' @import fuj
#' @docType package
#' @name mark
"_PACKAGE"

op.mark <- list( # nolint: object_name_linter.
  mark.author            = NULL,
  # control for check_interactive() to return interactive() or TRUE
  mark.check_interactive = TRUE,
  mark.note.width        = NULL,
  mark.todos.ext         = c("R", "Rmd", "qmd", "md", "py"),
  mark.todos.force       = FALSE,

  mark.days_in_month     = 30,
  mark.days_in_year      = 365,
  mark.weeks_inn_year    = 52,
  mark.default_tz        = "UTC",
  mark.na_list           = na_list
)
