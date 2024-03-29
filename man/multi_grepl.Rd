% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searches.R
\name{multi_grepl}
\alias{multi_grepl}
\alias{multi_grep}
\title{Multiple searching}
\usage{
multi_grepl(x, patterns, ..., simplify = TRUE)

multi_grep(x, patterns, ..., simplify = TRUE)
}
\arguments{
\item{x}{Passed to \code{\link[base:grep]{base::grepl()}}}

\item{patterns}{A list or vector of patterns to search across \code{x}; if named
value returned will be the name of the pattern -- otherwise the position.
Pattern match reported will be the first in the list that is found}

\item{...}{Additional arguments passed to \code{\link[base:grep]{base::grepl()}}}

\item{simplify}{if \code{FALSE} will return a list of all matches, otherwise the
first match found}
}
\value{
The name or position of the pattern that is matched
}
\description{
Multiple search pattern searches
}
\examples{
x <- c("apple", "banana", "lemon")
multi_grepl(x, c("a" = "^[ab]", "b" = "lem"))
multi_grepl(x, c("a" = "^[ab]", "b" = "q"))                   # lemon not matches on either
multi_grepl(x, c("a" = "^[ab]", "b" = "e"))                   # apple matches "a" before "b"
multi_grepl(x, c("a" = "^[ab]", "b" = "e"), simplify = FALSE) # shows all matches
multi_grepl(x, c("^[ab]", "e"))                               # returned as positions
multi_grepl(x, c("^[ab]", "e"), simplify = FALSE)

}
