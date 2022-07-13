#' Find closest match in a vector
#'
#' @param x numeric vector 
#' @param y vector of numeric values, each of which be matched with the closest
#'     value in \code{x}
#'
#' @return numeric vector of values of \code{x} that are closest to each 
#'     element of \code{y}
#' 
#' @export
#' 
closestMatch <- function(x, y) {
    unlist(lapply(y, function(i) {
    x[which(abs(x-i) == min(abs(x-i)))]
  }))
}
