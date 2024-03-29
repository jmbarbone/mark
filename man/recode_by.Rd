% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{recode_by}
\alias{recode_by}
\alias{recode_only}
\title{Recode by}
\usage{
recode_by(x, by, vals = NULL, mode = "any")

recode_only(x, by, vals = NULL)
}
\arguments{
\item{x}{A vector to recode}

\item{by}{A names vector (\code{new = old}); any non-matching values are set to
the appropriate \code{NA}}

\item{vals}{An optional vector of values to use in lieu of a names in the
vector; this takes priority over \code{names(by)}.  This can be the same length
as \code{by} or a single value.}

\item{mode}{passed to \code{as.vector()}}
}
\value{
A vector of values from \code{x}
}
\description{
A simple implementation of recoding
}
\details{
This can be comparable to \code{\link[dplyr:recode]{dplyr::recode()}} expect that the values are
arranged as \code{new = old} rather than \code{old = new} and allows for a separate
vector to be passed for \code{new}.

\code{recode_only()} will only recode the values matches in \code{by}/\code{val}.
The \code{mode} is automatically set according to \code{mode(x)}.  This functions
more like \code{\link[base:replace]{base::replace()}} but with extra features
}
\examples{
recode_by(1:3, c(a = 1, b = 2))
recode_by(letters[1:3], c(`1` = "a", `2` = "b"))                   # will not guess mode
recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer") # make as integer
recode_by(letters[1:3], c("a", "b"), vals = 1:2)                   # or pass to vals

recode_only(letters[1:3], c("zzz" = "a"))
recode_only(letters[1:3], c(`1` = "a")) # returns as "1"
recode_only(1:3, c("a" = 1))            # coerced to NA

# Pass list for multiples
recode_only(letters[1:10], list(abc = c("a", "b", "c"), ef = c("e", "f")))

}
\seealso{
\code{\link[dplyr:recode]{dplyr::recode()}}
}
