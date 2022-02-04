
na_list <- list(
  logical   = logical(),
  character = character(),
  integer   = integer(),
  double    = double(),
  numeric   = numeric(),
  Date      = as.Date(NA),
  POSIXct   = as.POSIXct(NA),
  POSIXlt   = as.POSIXlt(NA)[[1]]
)

options(mark.na_list = na_list)
