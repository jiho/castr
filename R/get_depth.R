#' Get the depth at index i
#'
#' Simply extracts element `i` of `depth` but includes failsafes for missing depth or missing `i`. Used by other functions.
#'
#' @param i element of `depth` to extract.
#' @inheritParams check_input
#'
#' @return If both `i` and `depth` are provided, return `depth[i]`. If `i` is NULL/empty, return NA. If `depth` is NULL return `i`.
#'
#' @examples
#' castr:::get_depth(2, c(34, 35, 36))
#' castr:::get_depth(2, NULL)
#' castr:::get_depth(NULL, c(34, 35, 36))
#' castr:::get_depth(c(), c(34, 35, 36))
get_depth <- function(i, depth) {
  if (length(i) > 0) {
    if (!is.null(depth)) {
      i <- depth[i]
    }
  } else {
    i <- NA
  }
  return(i)
}
