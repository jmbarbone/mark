% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array.R
\name{array_extract}
\alias{array_extract}
\title{Array extract}
\usage{
array_extract(.arr, ..., default = "1")
}
\arguments{
\item{.arr}{An array}

\item{...}{A named list by array dimension number and the value}

\item{default}{The default dimension index}
}
\value{
A value from the array \code{arr}
}
\description{
Extract dimensions from an array
}
\examples{
x <- array(rep(NA, 27), dim = c(3, 3, 3))
x[1, 2, 3] <- TRUE
x[1, 2, 3]
x
array_extract(x, `2` = 2, `3` = 3)
}
