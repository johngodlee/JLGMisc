#' Test whether all elements of multiple vectors are equal
#'
#' @param ... Any number of vectors, of identical length
#'
#' @return vector of logical elements
#' 
#' @examples
#' a <- c(1,2,3)
#' b <- c(1,2,3)
#' c <- c(1,2,4)
#' allEqual(a,b,c)
#' @export
#' 
allEqual <- function(...) {
	input_list <- list(...)
	check.eq <- sapply(input_list[-1], function(x) {x == input_list[[1]]})
	all(check.eq == TRUE)
}
