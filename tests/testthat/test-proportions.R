context("proportions")

library(dplyr)

test_that("Fake test", {

  # microbenchmark::microbenchmark(
  #   dplyr      = temp %>% count(Species, name = "prop") %>% mutate(prop = prop / sum(prop)),
  #   proportion = proportion(temp, Species),
  #   times      = 10,
  #   # check      = "equal",
  #   setup      = {temp <- as_tibble(iris)})

  expect_true(TRUE)
})