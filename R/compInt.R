#' Calculate compound interest by percentage
#'
#' @param v initial capital
#' @param r percentage annual rate of interest e.g. 5.81% = 0.0581
#' @param t time in number of years of investment
#'
#' @return numeric vector
#' 
#' @examples
#' compInt(v = 10, r = 0.1, t = 10)
#' @export
#' 
compInt <- function(v, r, t){
  v*(1 + r)^t
}
