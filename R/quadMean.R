#' Calculate the quadratic mean
#'
#' @param x vector 
#'
#' @return quadratic mean of vector
#' 
#' @examples
#' x <- seq(0, 10)
#' quadMean(x)
#' @export
#' 
quadMean <- function(x) {
  sqrt(mean(x^2))
}
