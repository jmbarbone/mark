% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md5.R
\name{md5}
\alias{md5}
\title{Compute the MD5 hash of an object}
\usage{
md5(x)
}
\arguments{
\item{x}{An object}
}
\value{
A \code{md5sum} object
}
\description{
Wrapper for calling \code{\link[tools:md5sum]{tools::md5sum()}} on objects rather than files.
}
\details{
All \code{x} objects are \link[base:serialize]{serialized} to a temporary
file before \code{\link[tools:md5sum]{tools::md5sum()}} is called.
}
\examples{
md5("hello")
md5(1:10)
md5(data.frame(a = 1:10, b = letters[1:10]))
}
