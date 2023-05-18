#' Round a number to nearest multiple of a given number
#'
#' @param x Numeric vector
#' @param y Multiple to round to
#' @param fun function for rounding, either \code{round}, \code{floor}, 
#'     \code{ceiling}
#'
#' @return Vector of rounded values
#' 
#' @examples
#' a <- seq(1, 20, 0.5)
#' roundAny(a, 2)
#' roundAny(a, 2, floor)
#' 
#' @export
#' 
roundAny <- function(x, y, fun = round) {
  fun(x/y)*y
}
