% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rleNA.R
\name{rleNA}
\alias{rleNA}
\title{\code{rle()} with NA values treated as real values}
\usage{
rleNA(x)
}
\arguments{
\item{x}{vector}
}
\value{
relative length encoding of vector
}
\description{
\code{rle()} with NA values treated as real values
}
\examples{
x <- c(0,0,0,NA,NA,NA,1,1,1)
rle(x)
rleNA(x)

}
