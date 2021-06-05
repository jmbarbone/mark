library(mark, warn.conflicts = FALSE)
library(bench)

n <- 1e4

year <- sample(0:3000, n, TRUE)
month <- sample(c("UN", "XX", month.abb), n, TRUE)
month[sample(1e4, 1e2)] <- sample(0:12, 1e2, TRUE)
day <- sample(0:32, n, TRUE)

ymd <- paste( year, month,  day, sep = "-")
dmy <- paste(  day, month, year, sep = "-")
mdy <- paste(month,   day, year, sep = "-")


bench::mark(
  date_from_partial(ymd),
  date_from_partial(dmy, "dmy"),
  date_from_partial(mdy, "mdy"),
  iterations = 5,
  check = FALSE
)

# bench:::autoplot.bench_mark(.Last.value)

