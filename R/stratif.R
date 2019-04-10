#' Compute an index of stratification
#'
#' Compute the difference in a given variable between a two depth layers, usually the surface, mixed layer and a bottom reference layer. This is often used with density to quantify the intensity of stratification.
#'
#' @inheritParams mld
#' @param min.depths depth(s) of reference, near the surface; when `min.depths` is a vector, the value of x is averaged between those depths.
#' @param max.depths similar to `min.depths` but at the bottom.
#' @param fun function used to summarise the values over `min.depths` and `max.depths`; typically this is `mean` but `median` could be used for a more robust estimate.
#'
#' @return The value of the stratification index.
#' @export
#'
#' @seealso [mld()] for the computation of the depth of the mixed layer, another quantification of the intensity of mixing/stratification; [smooth()] for smoothing.
#'
#' @examples
#' # estimate the intensity of stratification using density
#' plot(-depth ~ sigma, data=d, type="l")
#' abline(h=-c(1, 5, 61, 65), col="red")
#' stratif(d$sigma, d$depth, min.depths=1:5, max.depths=61:65)
#' stratif(d$sigma, d$depth, min.depths=1:5, max.depths=61:65, fun=median)
#'
#' # estimate the intensity of the peak of chlorophyll at the deep
#' # chlorophyll maximum
#' plot(-depth ~ chla, data=d, type="l")
#' DCM <- maxd(d$chla, d$depth, n.smooth=2)
#' abline(h=-c(0, 2, DCM-2, DCM+2), col="chartreuse4")
#' stratif(d$chla, d$depth, min.depths=0:2, max.depths=c(DCM-2, DCM+2))
stratif <- function(x, depth, min.depths, max.depths, n.smooth=0, k=2, fun=mean) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }

  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)

  # get indexes at which to consider data
  imin <- which(depth >= min(min.depths) & depth <= max(min.depths))
  imax <- which(depth >= min(max.depths) & depth <= max(max.depths))
  # TODO divide by depth range? add a standardize argument defaulting to FALSE?

  # compute the index
  if (!is.function(fun)) {
    fun <- get(as.character(substitute(fun)), mode="function")
  }
  abs(fun(x[imin], na.rm=TRUE) - fun(x[imax], na.rm=TRUE))
}
