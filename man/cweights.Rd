% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cweights.R
\name{cweights}
\alias{cweights}
\title{Compute a centered vector of weights}
\usage{
cweights(k, e = 1)
}
\arguments{
\item{k}{order of the window; the window size is 2k+1.}

\item{e}{exponent for the increase of weights towards the center. By default the increase in linear (e=1); e>1 makes for sharper focus on the center; e<1 spreads the focus to the rest of the window.}
}
\value{
A vector of weights.
}
\description{
Compute a centered vector of weights
}
\examples{
# Plot the distribution of weights
plot(cweights(4), type="b")
plot(cweights(4, 2), type="b")
plot(cweights(4, 0.1), type="b")

# Compare a simple moving average with a weighted moving average
set.seed(1)
x <- sin(1:100/10) + rnorm(100, sd=0.5)
plot(x)
xm <- slide(x, k=2, mean, na.rm=TRUE)
xwm <- slide(x, k=2, weighted.mean, w=cweights(k=2), na.rm=TRUE)
lines(xm, col="blue")
lines(xwm, col="red")
# The weighted version is a bit smoother
}
