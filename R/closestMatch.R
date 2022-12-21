#' Find closest match in a vector
#'
#' @param x numeric vector 
#' @param y vector of numeric values, each of which will be matched with the 
#'     closest value in \code{x}
#'
#' @return numeric vector of values of \code{x} that are closest to each 
#'     element of \code{y}
#' 
#' @examples
#' x <- seq(1, 5, 0.3)
#' y <- seq(0, 6, 0.25)
#' closestMatch(x, y)
#' 
#' @export
#' 
closestMatch <- function(x, y) {
    unlist(lapply(y, function(i) {
    x[which(abs(x-i) == min(abs(x-i)))]
  }))
}
