#' Scale a vector to between zero and one
#'
#' @param x numeric vector
#'
#' @return Numeric vector with values scaled between zero and one
#' 
#' @examples
#' scale01(1:1000)
#'  
#' @export
#' 
scale01 <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
