#' mark
#'
#' Miscellaneous, Analytic R Kernels
#'
#' @import fuj
#' @import cnd
#' @docType package
#' @name mark
"_PACKAGE"

# nolint next: object_name_linter.
op.mark <- list(
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
  mark.na_list           = list(
    logical   = logical(),
    character = character(),
    integer   = integer(),
    double    = double(),
    numeric   = numeric(),
    Date      = as.Date(NA),
    POSIXct   = as.POSIXct(NA),
    POSIXlt   = as.POSIXlt(NA)[[1]]
  )
)
