% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nas.R
\name{remove_na}
\alias{remove_na}
\alias{remove_na.default}
\alias{remove_na.list}
\alias{remove_na.factor}
\alias{remove_na.fact}
\title{Remove NA}
\usage{
remove_na(x)

\method{remove_na}{default}(x)

\method{remove_na}{list}(x)

\method{remove_na}{factor}(x)

\method{remove_na}{fact}(x)
}
\arguments{
\item{x}{A vector of values}
}
\value{
\code{x} without values where \code{is.na(x)} is \code{TRUE}
For factors, a new factor (\code{ordered} if \code{is.ordered(x)})
}
\description{
Remove NAs from a vector
}
\details{
\code{remove_na.factor} will remove \code{NA} values as identified by the \code{levels()}
or by the integer value of the level.  \code{factors} are recreated with all
\code{NA} values and, if present, the \code{NA} \code{level} removed.
}
\examples{
remove_na(c(4, 1, 2, NA, 4, NA, 3, 2))

# removes based on levels
remove_na(fact(c("b", NA, "a", "c")))

# removes based on values
x <- as_ordered(c("b", "d", "a", "c"))
x[2:3] <- NA
str(remove_na(x))
}
