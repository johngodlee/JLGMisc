#' Rotate a matrix 90 degrees clockwise or anti-clockwise
#'
#' @param x matrix
#' @param clockwise logical, if TRUE rotate clockwise, if FALSE rotate 
#'     anti-clockwise
#'
#' @return matrix
#' @export
#' 
rotate  <- function(x, clockwise = TRUE) {
  if (clockwise) { 
    t(apply(x, 2, rev))
  } else {
    apply(t(x), 2, rev)
  } 
}
