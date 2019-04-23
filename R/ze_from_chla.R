#' Compute the lower limit of the euphotic zone from integrated chlorophyll content
#'
#' @param chla vector of chlorophyll concentration, in mg m-3.
#' @param depth vector of depths at which `chla` is measured.
#' @param fit method use to represent the relationship between `Ze` and integrated chlorophyll.
#'
#' @details The lower limit of the euphotic zone (`Ze`) is normally computed from downwelling PAR irradiance (see [ze_from_par()]). But Morel (1988) modelled a strong correlation between integrated chlorophyll content and `Ze`. Morel and Berthon (1989) defined an approximation of this relationship (equations 1a and 1b). Morel and Maritorena (2001) provided an improved parameteriazation of the relationship, using either a piece-wise linear model or a polynomial one. This later parameterization is used here.
#'
#' The piecewise linear relationship is valid for `Ze` in `[10, 180]` while the polynomial one is valid for `Ze` in `[5, 180]`. The function returns `NA` outside those ranges.
#'
#' @return The depth of the lower limit of the euphotic zone.
#' @export
#'
#' @family functions computing remarkable depths
#' @seealso [interpolate()] to interpolate `chla` vertically to a greater precision (as advised by Morel and Maritorena, 2001), [smooth()] for smoothing.
#'
#' @references
#' Morel (1988). Optical modeling of the upper ocean in relation to its biogenous matter content (case I waters). Journal of Geophysical Research: Oceans, 93(C9), 10749-10768.
#'
#' Morel and Berthon (1989). Surface pigments, algal biomass profiles, and potential production of the euphotic layer: Relationships reinvestigated in view of remote‐sensing applications. Limnology and Oceanography, 34(8), 1545-1562.
#'
#' Morel and Maritorena (2001). Bio-optical properties of oceanic waters: A reappraisal. Journal of Geophysical Research: Oceans, 106(C4), 7163-7180. <https://doi.org/10.1029/2000JC000319>
#'
#' @examples
#' plot(-depth ~ chla, data=d, type="l")
#' Ze <- ze_from_chla(d$chla, d$depth)
#' abline(h=-Ze, col="chartreuse4")
#'
#' Ze <- ze_from_chla(d$chla, d$depth, fit="poly")
#' # same result
ze_from_chla <- function(chla, depth, fit=c("piecewise-linear", "polynomial")) {
  ok <- check_input(chla, depth)
  if (!ok) { return(NA) }

  # determine which fitting method to use
  fit <- match.arg(fit)

  df <- na.omit(data.frame(chla, depth))

  # compute integrated chlorophyll
  integrated_chla <- cumsum(df$chla)

  if (fit == "piecewise-linear") {
    ze1 <- 912.5*integrated_chla^(-0.839)
    ze2 <- 426.3*integrated_chla^(-0.547)
    ze <- ifelse(ze1 < 102, ze1, ze2)
    ze[ze < 10] <- NA
  } else {
    lchla <- log10(integrated_chla)
    lze <- 2.1236 + 0.932468*lchla - 1.4264*lchla^2 + 0.52776*lchla^3 - 0.07617*lchla^4
    ze <- 10^lze
    ze[ze < 5] <- NA
  }

  # the lower limit is the depth at which ze becomes < depth
  limit <- na.omit(ze < df$depth)
  if (any(limit)) {
    # when this happens, get the corresponding depth
    ze <- df$depth[min(which(limit))]
    # ze tends towards 183m in pure water and cannot physically be deepter
    # but the formula is only valid until 180m
    # so when ze is too deep, set it to the maximum
    if (ze > 180) {
      ze <- 180
    }    
  } else {
    # when this is never true, ze is deeper than the maximum depth (or is the theoretical maximum)
    ze <- ifelse(
      # if we get to the theoretical maximum, then ze set to it
      max(df$depth) > 180, 180,
      # otherwise, we don't know where ze is
      NA)
  }

  return(ze)
}
