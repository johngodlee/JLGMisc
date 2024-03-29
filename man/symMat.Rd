% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symMat.R
\name{symMat}
\alias{symMat}
\title{Transform a 3 column pairwise dataframe to a symmetric matrix}
\usage{
symMat(x, y, z)
}
\arguments{
\item{x}{becomes the rownames}

\item{y}{becomes the colnames}

\item{z}{becomes the content of the matrix}
}
\value{
matrix
}
\description{
Transform a 3 column pairwise dataframe to a symmetric matrix
}
\examples{
x <- c(2,3,4,3,4,4)
y <- c(1,1,1,2,2,3)
z <- seq_along(y)
symMat(x,y,z)

}
