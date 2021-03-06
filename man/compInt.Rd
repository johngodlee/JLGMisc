% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compInt.R
\name{compInt}
\alias{compInt}
\title{Calculate compound interest by percentage}
\usage{
compInt(v, r, t)
}
\arguments{
\item{v}{initial capital}

\item{r}{percentage annual rate of interest e.g. 5.81\% = 0.0581}

\item{t}{time in number of years of investment}
}
\value{
numeric vector
}
\description{
Calculate compound interest by percentage
}
\examples{
compInt(v = 10, r = 0.1, t = 10)
}
