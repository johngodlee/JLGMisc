#' Estimate optimal binwidth for histogram according to Freedman-Diaconis rule
#'
#' @param x numeric vector to be plotted on histogram
#'
#' @return numeric vector
#' 
#' @examples
#' x <- rnorm(100)
#' binwFD(x)
#' 
#' @export
#' 
binwFD <- function(x) { 
  2 * IQR(x) / length(x)^(1/3)
}
