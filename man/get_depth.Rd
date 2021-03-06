% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_depth.R
\name{get_depth}
\alias{get_depth}
\title{Get the depth at index i}
\usage{
get_depth(i, depth)
}
\arguments{
\item{i}{element of \code{depth} to extract.}

\item{depth}{vector of depths at which \code{x} is measured.}
}
\value{
If both \code{i} and \code{depth} are provided, return \code{depth[i]}. If \code{i} is NULL/empty, return NA. If \code{depth} is NULL return \code{i}.
}
\description{
Simply extracts element \code{i} of \code{depth} but includes failsafes for missing depth or missing \code{i}. Used by other functions.
}
\examples{
castr:::get_depth(2, c(34, 35, 36))
castr:::get_depth(2, NULL)
castr:::get_depth(NULL, c(34, 35, 36))
castr:::get_depth(c(), c(34, 35, 36))
}
