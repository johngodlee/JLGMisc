#' Calculate vector of hline positions for xtable's hline.after parameter
#'
#' @param ngroup number of groups in the table
#' @param nsize number of members in each group
#'
#' @return vector of hline positions 
#' 
#' @examples
#' xtableHline(5,4)
#' xtableHline(3,c(4,3,2))
#' 
#' @export
#' 
xtableHline <- function(ngroup, nsize) {
  if (length(ngroup) > 1 | !is.numeric(ngroup)) {
    stop("ngroup must be a numeric vector of length 1")
  }
  if (!is.numeric(nsize)) { 
    stop("nsize must be a numeric vector")
  }
  if (length(nsize) > 1 & length(nsize) != ngroup) {
    stop("if nsize is a vector its length must be identical to the value of ngroup")
  }

  if (length(nsize) == 1) {
    out <- c(-1, 0, 
      seq(from = nsize, by = nsize, length.out = ngroup -1), 
      ngroup*nsize)
  } else {
    out <- c(-1, 0, cumsum(nsize))
  }

  return(out)
}
