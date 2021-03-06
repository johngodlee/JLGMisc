% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findHull.R
\name{findHull}
\alias{findHull}
\title{Find the convex hull of a 2D set of points}
\usage{
findHull(dat, x, y, group = NULL)
}
\arguments{
\item{dat}{dataframe of points}

\item{x}{column name string of x axis values}

\item{y}{column name string of y axis values}

\item{group}{optional column name string of grouping variable}
}
\value{
dataframe of convex hulls as vertices
}
\description{
Find the convex hull of a 2D set of points
}
\examples{
dat <- data.frame(x = rnorm(50), y = rnorm(50), 
  group = sample(seq_len(5), 50, replace = TRUE))

findHull(dat, "x", "y", "group")

}
