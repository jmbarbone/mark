% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glob.R
\name{glob}
\alias{glob}
\title{Wildcard globbing}
\usage{
glob(x, pattern = NULL, value = TRUE, ...)
}
\arguments{
\item{x}{A vector of characters}

\item{pattern}{Wildcard globbing pattern}

\item{value, ...}{Additional parameters passed to \code{grep}. Note: \code{value} is by
default \code{TRUE}; when \code{NA}, \code{...} is passed to \code{grepl}.}
}
\description{
Helper function for globbing character vectors
}
\examples{
x <- c("apple", "banana", "peach", "pear", "orange")
glob(x, "*e")
glob(x, "pea*", value = FALSE)
glob(x, "*an*", value = NA)

path <- system.file("R", package = "mark")
glob(list.files(path), "r*")
}
