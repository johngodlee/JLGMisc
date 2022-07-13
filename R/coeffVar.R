#' Calculate the coefficient of variation
#'
#' @param x numeric vector
#'
#' @return coefficient of variation of vector
#' 
#' @examples
#' coeffVar(seq(0, 10))
#' @export
#' 
coeffVar <- function(x) {
  sd(x) / mean(x) * 100
}
