#' Calculate the standard error of the mean of a vector
#'
#' @param x numeric vector
#'
#' @return Standard error of the mean
#' 
#' @examples
#' x <- rnorm(100)
#' stdErrMean(x)
#' 
#' @export
#' 
stdErrMean <- function(x) {
  sd(x) / sqrt(length(x))
}
