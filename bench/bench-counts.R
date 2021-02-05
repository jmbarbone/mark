library(bench)
library(jordan, warn.conflicts = TRUE)

x <- quick_df(list(
  a = sample(letters, 1e4, TRUE),
  b = sample(letters, 1e4, TRUE),
  c = sample(letters, 1e4, TRUE)
))

n <- 1e4
a <- as.data.frame(matrix(stringi::stri_rand_strings(26 * n, 5), ncol = 26))
names(a) <- letters
b <- as.data.frame(matrix(stats::rpois(2 * n, 20), ncol = 2))
c <- as.data.frame(matrix(stats::runif(3 * n), ncol = 3))

x <- cbind(a, b, c)
names(x) <- make.unique(names(x))

bench::mark(
  counts(x, "a"),
  dplyr::count(x, a),

  counts(x, 1:3),
  dplyr::count(x, a, b, c),

  counts(x, c("z", "V1", "V1.1")),
  dplyr::count(x, z, V1, V1.1),
  iterations = 10,
  check = FALSE
)

# ggplot2::autoplot(.Last.value)
