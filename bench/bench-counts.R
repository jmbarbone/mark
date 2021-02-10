library(bench)
library(jordan, warn.conflicts = TRUE)

x <- quick_df(list(
  a = sample(letters, 1e4, TRUE),
  b = sample(letters, 1e4, TRUE),
  c = sample(letters, 1e4, TRUE)
))

n <- 1e4
xchars <- stringi::stri_rand_strings(26 * n, 5)
chars <- as.data.frame(matrix(xchars, ncol = 26))
names(chars) <- letters
ints <- as.data.frame(matrix(stats::rpois(2 * n, 20), ncol = 2))
dbls <- as.data.frame(matrix(stats::runif(3 * n), ncol = 3))
bools <- as.data.frame(matrix(sample(c(TRUE, FALSE, NA), n, TRUE)))
facts <- as.data.frame(matrix(factor(xchars, levels = sort(unique(xchars))), ncol = 26))

x <- cbind(chars, ints, dbls, bools, facts)
names(x) <- make.unique(names(x))

bench::mark(
  `vec characters` = counts(chars[[1]]),
  `vec factors` = counts(facts[[1]]),
  `vec factors 2` = counts(facts[[1]]),
  `vec integers` = counts(ints[[1]]),
  `vec doubles` = counts(dbls[[1]]),
  `vec boolean` = counts(bools[[1]]),

  `df single jordan` = counts(x, "a"),
  `df single dplyr` = dplyr::count(x, a),

  `df 3 cols jordan` = counts(x, 1:3),
  `df 3 cols dplyr` = dplyr::count(x, a, b, c),

  `df all cols jordan` = counts(x, seq_along(x)),
  `df all cols dplyr` = dplyr::count(dplyr::group_by_all(x), name = "N"),

  iterations = 10,
  check = FALSE
)

# ggplot2::autoplot(.Last.value)
