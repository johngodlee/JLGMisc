#' Calculate standard error on binomial data
#'
#' @param x TRUE FALSE vector
#'
#' @return standard error
#' 
#' @examples
#' a <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
#' stdErrBinom(a)
#' @export
#' 
stdErrBinom <- function(x) { 
  sqrt(
    ( sum(x)/length(x) ) * 
	( 1-(sum(x)/length(x)) ) / 
	  length(x) 
	)
}

