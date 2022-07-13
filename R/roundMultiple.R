#' Round a number to nearest multiple of a given number
#'
#' @param x Numveric vector
#' @param y Multiple to round to
#'
#' @return Vector of rounded values
#' 
#' @examples
#' a <- seq(1, 20, 0.5)
#' roundMultiple(a, 2)
#' @export
#' 
roundMultiple <- function(x, y) {
  sapply(x, function(z) {
    if ((y - z %% y) <= z %% y) { 
      z + (y - z %% y)
    } else { 
      z - (z %% y)
    }
  })
}


