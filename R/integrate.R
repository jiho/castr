#' Integrate data over a range of depths
#'
#' Sum or average a given variable over a range of depths.
#'
#' @inheritParams check_input
#' @param fun function used to perform the integration; usually `\link[base:sum]{sum()}` or `\link[base:mean]{mean()}`.
#' @param from depth at which to start the integration; by default 0.
#' @param to depth at which to stop the integration; by default 100 m. Must be greater than `from`.
#' @param ... passed to `\link[stats:approx]{approx()}`; allows to choose the method for the interpolation: "linear" (the default) results in integration by trapezoids, "constant" with f=0 or f=1 results in integration by rectangles (and is often not optimal).
#'
#' @details Integration is used to compute the stock of a quantity (Chlorophyll a, nutrients, etc.) in a given depth layer (when `fun` is `\link[base:sum]{sum()}`) of the average of a variable (temperature, density, etc.) in a layer (in which case, `fun` should be `\link[base:mean]{mean()}`). It is often used with discrete data coming from bottle samples and needs to be made continuous. Here, this is done by interpolating the data every meter, either using either linear or constant interpolation.
#'
#' @return The value of the variable, integrated over the depth range.
#' @export
#'
#' @examples
#' # Stock of Chl a inferred from fluorescence
#' plot(-depth ~ fluo, data=d, type="l")
#'
#' # near the surface
#' integrate(d$fluo, d$depth, from=0, to=50)
#' integrate(d$fluo, d$depth, from=0, to=50, method="constant")
#'
#' # around the Deep Chlorophyll Maximum
#' DCM <- maxd(d$fluo, depth=d$depth)
#' integrate(d$fluo, d$depth, from=DCM-10, to=DCM+10)
#' # which amounts to this quantity
#' d_DCM <- subset(d, depth > DCM-10 & depth < DCM+10)
#' polygon(
#'   x=c(d_DCM$fluo, rep(0, times=nrow(d_DCM))),
#'   y=-c(d_DCM$depth, rev(d_DCM$depth)),
#'   col="chartreuse2", border=NA
#' )
#' abline(h=-DCM, col="chartreuse4")
#'
#' # For variables which do not represent a quantity, it is usually more
#' # meaningful to compute the mean.
#' # Average temperature in the top 50 m
#' plot(-depth ~ temp, data=d, type="l")
#' integrate(d$temp, d$depth, from=0, to=50, fun=mean)
#' # or in the surface, mixed layer
#' MLD <- mld(d$sigma, d$depth)
#' abline(h=-MLD, col="red")
#' integrate(d$temp, d$depth, from=0, to=MLD, fun=mean)
#'
#' # This also work with discrete data, over a few irregularly spaced depths
#' # (like those coming from the analysis of water samples in bottles)
#' d <- data.frame(depth=c(10, 20, 50, 150), chla=c(0.1, 0.2, 0.4, 0.001))
#' plot(-depth ~ chla, data=d, type="b")
#' integrate(d$chla, d$depth, from=0, to=100, fun=sum)
integrate <- function(x, depth, fun=sum, from=0, to=100, ...) {
  if ( (sum(!is.na(x) & !is.na(depth)) < 2)){
    warning("Not enough non-missing data to integrate, returning NA")
    y <- NA
  } else {
    if ( any(is.na(c(from, to))) ) {
      warning("At least one integration bound is undefined, returning NA")
      y <- NA
    } else {
      if ( from > to ) {
        warning("from > to, inverting integration range")
        range <- rev(c(from, to))
        from <- range[1]
        to <- range[2]
        rm(range)
      }
      # perform the integration
      y <- fun(stats::approx(x=depth, y=x, xout=seq(from, to, by=1), rule=2, ...)$y, na.rm=TRUE)
    }
  }
  return(y)
}
