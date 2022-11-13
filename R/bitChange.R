#' Detect when the values in an ordered vector change
#'
#' @param x vector
#'
#' @return integer vector where the value increases each time the value in 
#'     \code{x} changes.
#' 
#' @examples
#' x <- seq_len(5)
#' all(bitChange(x) == x)
#' x <- sample(seq_len(5), 20, replace = TRUE)
#' bitChange(x)
#' 
#' @export
#' 
bitChange <- function(x) {
  cumsum(c(1, diff(x) != 0))
}

