options(tidyverse.quiet = TRUE)

library(dplyr, warn.conflicts = FALSE)

out <- iris %>%
  tibble::as_tibble() %>%
  mutate(new_col = sqrt(Sepal.Width^2 + Sepal.Length^2)) %>%
  head(10)

a_litte_note <- "You're doing okay"

out

message("Message: Hello, there")
print("Printed: Why, hello there")
