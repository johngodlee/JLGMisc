#' Split a vector into chunks of given size
#'
#' @param x vector to split
#' @param n size of chunks
#'
#' @return list of length \code{n} 
#' 
#' @examples 
#' x <- seq_len(64)
#' n <- 8
#' chunkSplit(x,n)
#' 
#' @export
#' 
chunkSplit <- function(x, n) { 
  split(x, ceiling(x / n))
}

