#' Detect spikes in a cast
#'
#' Use the distance from a moving median as a criterion to find spikes.
#'
#' @details
#' A moving median is computed. Spikes are detected as large anomalies compared to the moving median. "Large" is defined as greater than `mult` times the median absolute deviation, computed over the same window (see `\link[stats]{mad}()`). The point(s) marked as spike(s) is(are) masked and the process is repeated, for at most `n.max` iterations.
#'
#' Because the criterion is based on a moving median, the function can detect spikes in a variable for which the mean value varies along the cast (as opposed to just flagging the most extreme values). For example, in a variable that varies between 0 and 5, a value of only 2 can be flagged as a spike if its surrounding values in the window are all close to 0.
#'
#' Because the deviation is gauged by the median absolute deviation, the function should handle variables in which variance varies along the cast. In other words, it should be able to detect small spikes in a region of low variance and large spikes in a region of high variance, without flagging everything as a spike in regions of high variance.
#'
#' NB: The default value of 5.3 for `mult` flags only points outside of a 95\% confidence interval for the mean, in the case of normally distributed data; but it should certainly be altered depending on the noise in your data.
#'
#' @inheritParams rollapply
#' @param mult number by which to multiply the mad to set the threshold to flag a point as a spike.
#' @param n.max maximum number of times to iterate over the data to find spikes.
#'
#' @return A logical vector, as long as `x`, containing TRUE for points that are considered as spikes, FALSE for others.
#' @export
#'
#' @seealso despike
#'
#' @examples
#' # create fake data
#' n <- 200
#' x <- seq(0, 3*2*pi, length=n)
#' y <- sin(x) * 0.5
#' # add noise, more in the second half of the data
#' set.seed(1)
#' y <- y + c(runif(n/2, -0.2, 0.2), runif(n/2, -0.5, 0.5))
#' # add 15 spikes
#' idx_spikes <- round(runif(15)*n)
#' y[idx_spikes] <- y[idx_spikes] + 0.5
#' # plot the data
#' # identify real spikes by crosses and detected spikes by red circles
#' df <- data.frame(x, y)
#' plot(y ~ x, data=df, type="l")
#' abline(v=x[100])
#' points(y ~ x, data=df[idx_spikes,], pch=4)
#' points(y ~ x, data=df[spikes(df$y, k=3, mult=2),], col="red")
#' # NB:
#' # The data varies (3 oscillations), so the window needs to be made
#' # small enough to follow the oscillations but not too small to avoid
#' # being influenced by the spikes.
#' # The data is quite noisy to start with, so the multiplier for the median
#' # absolute deviation needs to be small enough.
#' # All spikes are detected in the first, low noise half. Spikes of the
#' # same amplitude in the second half are often part of the noise and are
#' # not detected as spikes, for most of them (there are some false
#' # positive). True spikes can be masked in the noise but those that can
#' # reasonnably be expected to be detected (that stand out), are.
spikes <- function(x, k, mult=5.3, n.max=10) {

  # initialise with no spikes
  spikes <- rep(FALSE, times=length(x))

  # repeat spike detection at most n.max times
  for (i in 1:n.max) {
    # compute the anomaly to the median
    mmed <- rollapply(x, k=k, stats::median, na.rm=TRUE, n=3)
    anom <- x - mmed
    # detect when this anomaly is larger than a threshold based on the inherent variability of the data
    mmad <- rollapply(x, k=k*2, stats::mad, na.rm=TRUE)
    # NB: make the window larger here to better capture the local variability
    s <- (abs(anom) > mmad * mult) & (mmad > 0)

    # when no new spikes are detected, stop
    if (!any(s, na.rm=TRUE)) { break }

    # add the new spikes to the ones detected before
    spikes <- spikes | s
    # and remove the spiking values from the data
    x[spikes] <- NA
  }

  return(spikes)
}

#' Replace spikes by NA in a cast
#'
#' @inheritParams spikes
#' @param ... passed to [spikes()]
#' @export
#' @seealso spikes
despike <- function(x, ...) {
  x[spikes(x, ...)] <- NA
  return(x)
}
