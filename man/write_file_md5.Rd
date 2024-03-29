% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{write_file_md5}
\alias{write_file_md5}
\title{Write file with md5 hash check}
\usage{
write_file_md5(
  x,
  path = NULL,
  method = mark_write_methods(),
  overwrite = NA,
  quiet = FALSE,
  encoding = "UTF-8",
  compression = getOption("mark.compress.method", "default"),
  ...
)
}
\arguments{
\item{x}{An object to write to file}

\item{path}{The file or connection to write to (dependent on part by method)}

\item{method}{The method of saving the file.  When \code{default}, the method is
determined by file extension of \code{path}, if present, otherwise by the type
of object of \code{x}.}

\item{overwrite}{When \code{NA}, only saves if the md5 hashes do not match.
Otherwise, see \code{\link[fs:copy]{fs::file_copy()}}.}

\item{quiet}{When \code{TRUE}, suppresses messages from md5 checks.}

\item{encoding}{The encoding to use when writing the file.}

\item{compression}{The compression method to use when writing the file.}

\item{...}{Additional arguments passed to the write function.}
}
\value{
\code{x}, invisibly.  When \code{path} is not the \code{stdout()}, \code{x} is returned
with the attribute \code{"path"} set to the result of \code{\link[=file_copy_md5]{file_copy_md5()}}.
}
\description{
Write file with md5 hash check
}
\examples{
# just writes to stdout()
df <- data.frame(a = 1, b = 2)
write_file_md5(df)

temp <- tempfile()
write_file_md5(df, temp) # new
write_file_md5(df, temp) # same
df$c <- 3
write_file_md5(df, temp) # changes
fs::file_delete(temp)
}
