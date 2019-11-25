#' Replace erroneous data by NA
#'
#' @param x input data.frame
#' @param ... condition(s) that define bad data, which will be replaced by NA. Each condition is treated separately and all variables used in the condition are affected by the NA replacement.
#'
#' @return The input data with some elements replaced by NA
#' @export
#'
#' @examples
#' x <- read.csv(text="
#' pressure,salinity,sigma theta
#'        1,      38,         29
#'        2,      -2,          2
#'        3,      56,         28
#'        4,      37,         35",
#' check.names=FALSE)
#'
#' # remove negative salinities
#' censor(x, salinity < 0)
#' # variable names with special characters (e.g. spaces) need to be quoted
#' censor(x, `sigma theta` < 10)
#'
#' # two conditions that affect the same column can be defined at once
#' censor(x, salinity < 0 | salinity > 40)
#' # or one after the other
#' censor(x, salinity < 0, salinity > 40)
#'
#' # but when several names appear in a single condition,
#' # all columns are affected.
#' censor(x, salinity < 0 | `sigma theta` > 32)
#' # and the result is therefore different from
#' censor(x, salinity < 0 , `sigma theta` > 32)
#' # so, to affect column B based on a condition in column A only,
#' # use a dummy, always TRUE, condition for B, just so that it appears
#' censor(x, salinity < 0 & `sigma theta` > -Inf)
censor <- function(x, ...) {
  # library("rlang")
  # library("stringr")

  # get names of variables in x
  vars <- names(x)

  # get a list of all conditions as "calls"
  # dots <- quos(...)
  dots <- eval(substitute(alist(...)))

  for (cond in dots) {
    # detect which variable(s) this condition applies to
    # cond_text <- quo_text(cond)
    # cond_vars <- vars[str_detect(cond_text, vars)]
    cond_text <- deparse(cond)
    cond_vars <- vars[sapply(vars, function(x) {grepl(x, cond_text)})]

    # evaluate the condition in the context of the data `x` and replace values matching the condition by NA for all variables the condition applies to
    # x[which(eval_tidy(cond, data=x)), cond_vars] <- NA
    x[which(eval(cond, envir=x)), cond_vars] <- NA
  }

  return(x)
}
