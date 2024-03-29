% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_expand_seq.R
\name{fct_expand_seq}
\alias{fct_expand_seq}
\title{Factor Expand by Sequence}
\usage{
fct_expand_seq(
  x,
  min_lvl = min(x, na.rm = TRUE),
  max_lvl = max(x, na.rm = TRUE),
  by = 1L
)
}
\arguments{
\item{x}{An ordered factor}

\item{min_lvl}{The start of the level sequence}

\item{max_lvl}{The end of the level sequence}

\item{by}{Integer, number of steps in between}
}
\value{
An \code{ordered} vector
}
\description{
Expands an ordered factor from one level to another
}
\details{
Defaults for \code{min_lvl} and \code{max_lvl} are the minimum and maximum
levels in the ordered vector \code{x}.
}
\examples{
x <- ordered(letters[c(5:15, 2)], levels = letters)
fct_expand_seq(x)
fct_expand_seq(x, "g", "s", 3L) # from "g" to "s" by 3
fct_expand_seq(x, "g", "t", 3L) # same as above

# from the first inherit level to the last observed
fct_expand_seq(x, min(levels(x)))
}
