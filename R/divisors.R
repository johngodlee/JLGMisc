#' Find divisors of an integer
#'
#' @param x an integer
#'
#' @return vector of integer divisors
#' 
#' @export
#' 
divisors <- function(x){
  y <- seq_len(x)
  y[ x%%y == 0 ]
}

