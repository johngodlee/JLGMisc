#' Find values not shared between two vectors
#'
#' @param x vector
#' @param y vector
#'
#' @return vector of values not present in both vectors
#' 
#' @examples
#' x <- c(1,2,3,4)
#' y <- c(1,3,5,7)
#' outersect(x, y)
#' 
#' @export
#' 
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
