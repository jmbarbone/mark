library(plyr)
library(jordan, warn.conflicts = FALSE)
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

bench::mark(                #  k iters/second
  as.data.frame(x),         #  3
  plyr::quickdf(x),         # 25
  jordan::quick_df(x)       # 70
)

df <- quick_df(x)

bench::mark(
  jordan::complete_cases(df), # 26
  tidyr::drop_na(df)          # 18
)

bench::mark(
  jordan::complete_cases(df, c("a", "e", "d")), # 29
  tidyr::drop_na(df, a, e, d)                   # 21
)

df$bad <- NA

bench::mark(
  jordan::complete_cases(df),        # 23
  jordan::complete_cases(df, "bad"), # 92
  tidyr::drop_na(df),                # 18
  tidyr::drop_na(df, bad)            # 45
)
