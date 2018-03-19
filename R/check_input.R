#' Check input data
#'
#' Check that the input data is usable as a CTD cast (not fully missing and of compatible length). Used by other functions.
#'
#' @param x vector of the variable of interest.
#' @param depth vector of depths at which `x` is measured.
#'
#' @return TRUE when the data is ok, FALSE when it is fully missing, FALSE and throws an error when `depth` is not the same length as `x`.
#' @examples
#' check_input(c(22.1, 23.4, 23.2), c(1, 2, 3))
#' check_input(c(NA, NA, NA), c(1, 2, 3))
#' \dontrun{
#' check_input(c(22.1, 23.4, 23.2), c(1, 2))}
check_input <- function(x, depth=NULL) {
  ok <- TRUE
  # check the input
  if (all(is.na(x))) {
    ok <- FALSE
  }
  if (!is.null(depth)) {
    if (length(depth) != length(x)) {
      ok <- FALSE
      stop("The vector of data (n=", length(x), ") should be as long as the vector of depths (n=", length(depth), ")")
    }
  }
  return(ok)
}
