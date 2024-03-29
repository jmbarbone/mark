% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/switch.R
\name{switch-ext}
\alias{switch-ext}
\alias{switch_params}
\alias{switch_in_case}
\alias{switch_case}
\title{Switch with a list of parameters}
\usage{
switch_params(x, ...)

switch_in_case(x, ..., .default = NULL, .envir = parent.frame())

switch_case(..., .default = NULL, .envir = parent.frame())
}
\arguments{
\item{x}{A vector of values}

\item{...}{Case evaluations (named for \code{switch_params})}

\item{.default}{The default value if no matches are found in \code{...}
(default: \code{NULL} produces an \code{NA} value derived from \code{...})}

\item{.envir}{The environment in which to evaluate the LHS of \code{...} (default:
\code{parent.frame()})}
}
\value{
A named vector of values of same length \code{x}; or for \code{switch_case},
an unnamed vector of values matching the rhs of \code{...}

Inspired from:
\itemize{
\item https://stackoverflow.com/a/32835930/12126576
\item https://github.com/tidyverse/dplyr/issues/5811
}
}
\description{
\code{switch_params()} is a vectorized version of \code{switch}
\code{switch_case()} uses a formula syntax to return the value to the right of the
tilde (\code{~}) when \code{x} is \code{TRUE}
\code{switch_in_case()} is a special case of \code{switch_case()} for \code{match()}-ing \code{x}
in the values on the left to return the value on the right.
}
\details{
Switch with a list of params
}
\examples{
# by single
switch_params(c("j", "m", "b"), j = 10, b = 2, m = 13)


# match with TRUE
switch_case(
  1:10 == 9      ~ NA_integer_,
  1:10 \%\% 3 == 0 ~ 1:10,
  1:10 \%\% 4 == 0 ~ 11:20,
  1:10 \%\% 5 == 0 ~ 21:30,
  1:10 \%\% 2 == 0 ~ 31:40,
  .default = -1L
)

# match within a vector
switch_in_case(
  c(1, 2, 12, 4, 20, 21),
  1:10  ~ 1,
  11:20 ~ 2
)

switch_in_case(
  c("a", "b", "d", "e", "g", "j"),
  letters[1:3] ~ "a",
  letters[5:6] ~ "e"
)

use_these <- c(1, 3, 2, 5)
switch_in_case(
  1:10,
  use_these ~ TRUE,
  .default = FALSE
)

ne <- new.env()
ne$use_these2 <- use_these
# error
try(switch_in_case(
  1:10,
  use_these2 ~ TRUE
))
switch_in_case(
  1:10,
  use_these2 ~ TRUE,
  .envir = ne
)

switch_in_case(
  seq.int(1, 60, 6),
  1:10          ~ "a",
  11:20         ~ "b",
  c(22, 24, 26) ~ "c",
  30:Inf        ~ "d"
)

# Use functions
switch_in_case(
  1:6,
  c(1, 3, 5) ~ exp,
  c(2, 4) ~ log
)
}
