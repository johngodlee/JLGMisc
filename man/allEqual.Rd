% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allEqual.R
\name{allEqual}
\alias{allEqual}
\title{Test whether all elements of multiple vectors are equal}
\usage{
allEqual(...)
}
\arguments{
\item{...}{Any number of vectors, of identical length}
}
\value{
vector of logical elements
}
\description{
Test whether all elements of multiple vectors are equal
}
\examples{
a <- c(1,2,3)
b <- c(1,2,3)
c <- c(1,2,4)
allEqual(a,b,c)
}
