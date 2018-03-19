#' Detect the depth of the mixed layer
#'
#' @param x vector of the variable of interest, usually potential or in situ density.
#' @param depth vector of depths at which `x` is measured.
#' @param ref.depths depth(s) of reference, near the surface; when `ref.depths` is a vector, the value of `x` is averaged between those depths.
#' @param criteria value(s) considered as thresholds for the computation of the depth of the mixed layer. The thresholds are tried successively.
#' @param default.depth when no threshold is crossed, return this value instead; a usual value is the full depth of the water column (or `max(depth)`).
#' @param n.smooth integer, number of times to smooth the data before applying the mixed layer criteria.
#'
#' @details The mixed layer depth (MLD) is detected following de Boyer Montegut et al (2004): a reference value for density is taken near the surface and the water column is considered to be mixed until the depth at which density deviates from this refence by more than 0.03 kg/m^3. Optionally, when no depth satisfies this criterion, a second criterion of 0.01 kg/m^3 can be considered (as is by default).
#'
#' In addition, here, when a range of depths is provided as reference, the refence density is the average of the densities recorded in this depth range.
#'
#' @return If no criterion is met, return `default.depth`. If a criterion is met return the last depth at which it was not.
#' @export
#'
#' @family functions computing remarkable depths
#' @seealso [smooth()] for smoothing.
#'
#' @references
#' de Boyer Montegut et al (2004). Mixed layer depth over the global ocean: An examination of profile data and a profile‐based climatology. Journal of Geophysical Research: Oceans, 109(C12). <https://doi.org/10.1029/2004JC002378>
#'
#' @examples
#' plot(-depth ~ sigma, data=d, type="l")
#' # Compute mixed layer depth
#' MLD <- mld(d$sigma, d$depth)
#' abline(h=-MLD, col="red")
#' # Compare with the depth of the pycnocline
#' pycnocline <- clined(d$sigma, d$depth)
#' abline(h=-pycnocline, col="blue")
mld <- function(x, depth, ref.depths=5:10, criteria=c(0.03, 0.01), default.depth=NA, n.smooth=0, k=2, ...) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }

  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)

  # compute the reference value
  iref <- which(depth >= min(ref.depths) & depth <= max(ref.depths))
  xref <- mean(x[iref], na.rm=TRUE)
  if (is.na(xref)) {
    warning("No data at reference depth(s).")
    m <- NA
  } else {
    for (crit in criteria) {
      i <- which(x > (xref + crit) & depth > max(ref.depths)) - 1
      # NB: we want the element previous to meeting this criterion
      if (length(i) > 0) {
        i <- min(i)
        break
      }
    }

    # extract the corresponding depth
    m <- get_depth(i, depth)

    # replace by the default value when no criterion is met
    if (is.na(m)) {
      m <- default.depth
    }
  }

  return(m)
}
