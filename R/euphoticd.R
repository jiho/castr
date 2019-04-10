#' Detect the lower limit of the euphotic zone
#'
#' @param chla vector of chlorophyll concentration, in mg m-3.
#' @param depth vector of depths at which `chl` is measured.
#' @param fit method use to represent the relationship between Ze and integrated chlorophyll.
#'
#' @details The method is that of Morel and Berthon (1989) with the new parameter values from Morel and Maritorena (2001)
#'
#' @return The depth of the lower limit of the euphotic zone.
#' @export
#'
#' @family functions computing remarkable depths
#' @seealso [smooth()] for smoothing.
#'
#' @references
#' Morel and Berthon (1989). Surface pigments, algal biomass profiles, and potential production of the euphotic layer: Relationships reinvestigated in view of remote‐sensing applications. Limnology and Oceanography, 34(8), 1545-1562.
#'
#' Morel and Maritorena (2001). Bio-optical properties of oceanic waters: A reappraisal. Journal of Geophysical Research: Oceans, 106(C4). <https://doi.org/10.1029/2000JC000319>
#'
#' @examples
#' plot(-depth ~ chla, data=d, type="l")
#' Ze <- euphoticd(chla, depth)
#' abline(h=-Ze, col="chartreuse4")
#' Ze <- euphoticd(chla, depth, fit="poly")
#' # -> same result
euphoticd <- function(chla, depth, fit=c("piecewise-linear", "polynomial")) {
  # determine which fitting method to use
  fit <- match.arg(fit)

  # compute integrated chlorophyll
  integrated_chla <- cumsum(chla)

  if (fit == "piecewise-linear") {
    ze1 <- 912.5*integrated_chla^(-0.839)
    ze2 <- 426.3*integrated_chla^(-0.547)
    ze <- ifelse(ze1 < 102, ze1, ze2)
    ze <- ifelse(ze < 10 | ze > 180, NA, ze)
  } else {
    lchla <- log10(integrated_chla)
    lze <- 2.1236 + 0.932468*lchla - 1.4264*lchla^2 + 0.52776*lchla^3 - 0.07617*lchla^4
    ze <- 10^lze
    ze <- ifelse(ze < 5 | ze > 180, NA, ze)
  }

  # the thickness is the depth at which ze/z becomes < 1
  ze <- depth[min(which(ze < depth))]

  return(ze)
}
