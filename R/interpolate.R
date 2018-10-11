#' Interpolate values
#'
#' @param x coordinate where data is avalaible.
#' @param y vector of values measured at points `x`.
#' @param xout vector giving the coordinates at which to interpolate.
#' @param method interpolation method. When `y` is numeric (or coercible as such), the method can be "constant", "linear", or "spline". When `y` is discrete (factor, character, etc.), "constant" (i.e. nearest neighbour) interpolation is always performed, no mater what is specified.
#' @param extrapolate when `TRUE`, the default, extrapolate `y` for values of `xout` outside of `range(x)`.
#'
#' @return The vector of interpolated values at points `xout`.
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' # interpolate numerical values
#' x <- jitter(1:10, 2)
#' y <- runif(10, 10, 100)
#' xout <- 1:10
#' plot(x, y, type="b")
#' rug(x)
#' rug(xout, col="red")
#' points(xout, interpolate(x, y, xout, method="constant"), col="red")
#' points(xout, interpolate(x, y, xout, method="linear"), col="darkgreen")
#' points(xout, interpolate(x, y, xout, method="spline"), col="blue")
#'
#' # interpolate a factor
#' x <- jitter(1:10, 2)
#' y <- factor(letters[1:2][round(runif(10, 1, 2))])
#' xout <- 1:10
#' plot(x, y, yaxt="n", ylim=c(0,3))
#' axis(2, at=1:2, labels=levels(y))
#' rug(x)
#' rug(xout, col="red")
#' points(xout, interpolate(x, y, xout, method="spline"), col="red")
interpolate <- function(x, y, xout, method="linear", extrapolate=TRUE) {
  # remove NAs in input
  # they are useless for interpolation and cause trouble with contstant interpolation: the nearest neighbour picked may be NA
  d <- stats::na.omit(data.frame(x, y))
  interpol(d$x, d$y, xout=xout, method=method, extrapolate=extrapolate)
}

interpol <- function(x, y, xout, method="linear", extrapolate=TRUE) {
  UseMethod("interpol", y)
}

interpol.default <- function(x, y, xout, method="linear", extrapolate=TRUE) {
  method <- match.arg(method, choices=c("constant", "linear", "spline"))
  fi <- switch(method,
    constant = function(xout) {y[which.closest(from=xout, within=x, outside=extrapolate)]},
    linear = stats::approxfun(x, y, method="linear", rule=ifelse(extrapolate, 2, 1)),
    spline = stats::splinefun(x, y)
  )

  fi(xout)
}

interpol.factor <- function(x, y, xout, method="constant", extrapolate=TRUE) {
  y[which.closest(from=xout, within=x, outside=extrapolate)]
}

interpol.character <- interpol.factor