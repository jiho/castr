#' Compute a centered vector of weights
#'
#' @inheritParams slide
#' @param e exponent for the increase of weights towards the center. By default the increase in linear (e=1); e>1 makes for sharper focus on the center; e<1 spreads the focus to the rest of the window.
#'
#' @return A vector of weights.
#' @export
#'
#' @examples
#' # Plot the distribution of weights
#' plot(cweights(4), type="b")
#' plot(cweights(4, 2), type="b")
#' plot(cweights(4, 0.1), type="b")
#'
#' # Compare a simple moving average with a weighted moving average
#' set.seed(1)
#' x <- sin(1:100/10) + rnorm(100, sd=0.5)
#' plot(x)
#' xm <- slide(x, k=2, mean, na.rm=TRUE)
#' xwm <- slide(x, k=2, weighted.mean, w=cweights(k=2), na.rm=TRUE)
#' lines(xm, col="blue")
#' lines(xwm, col="red")
#' # The weighted version is a bit smoother
cweights <- function(k, e=1) {
  # compute the first half
  p <- seq(1,k)^e
  # compute the full vector
  x <- c(p, (k+1)^e, rev(p))
  # scale to sum to 1
  x / sum(x)
}