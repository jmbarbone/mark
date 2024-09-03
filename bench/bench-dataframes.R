library(plyr)
library(mark, warn.conflicts = FALSE)
library(bench)

x <- list(
  a = 1:1e6,
  b = 1:1e6,
  c = sample(letters, 1e6, TRUE),
  d = Sys.Date() + 1:1e6,
  e = Sys.time() - 1:1e6,
  f = runif(1e6)
)

for (i in seq_along(x)) {
  len <- length(x[[1]])
  x[[i]][sample(len, ceiling(len * runif(1)))] <- NA
}

df <- quick_df(x)
df$bad <- NA

res <- bench::mark(                            # median
  as.data.frame(x),                     #  305us
  plyr::quickdf(x),                     #   37us
  quick_df(x),                          #   21us

  complete_cases(df),                   #   42ms
  tidyr::drop_na(df),                   #   86ms

  complete_cases(df, c("a", "e", "d")), #   26ms
  tidyr::drop_na(df, a, e, d),          #   50ms

  complete_cases(df),                   #   53ms
  tidyr::drop_na(df),                   #   79ms
  complete_cases(df, "bad"),            #   12ms
  tidyr::drop_na(df, bad),              #   31ms

  iterations = 200,
  check = FALSE
)

res2 <- res
res2$expression <- fact(as.character(res2$expression))
ggplot2::autoplot(res2)
