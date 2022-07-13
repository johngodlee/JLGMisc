#' Transform a 3 column pairwise dataframe to a symmetric matrix
#'
#' @param x becomes the rownames
#' @param y becomes the colnames
#' @param z becomes the content of the matrix
#'
#' @return matrix
#' 
#' @examples
#' x <- c(2,3,4,3,4,4)
#' y <- c(1,1,1,2,2,3)
#' z <- seq_along(y)
#' symMat(x,y,z)
#' 
#' @export
#' 
symMat <- function(x, y, z) {
  lev <- sort(unique(c(x, y)))

  combs <- apply(data.frame(x, y), 1, function(i) { 
    paste(sort(i), collapse = "-") 
  })

  poss_combs <- apply(combn(lev, 2), 2, paste, collapse = "-") 

  if (!all(poss_combs %in% combs)) {
    stop("Not all pairs represented")
  }

  dat <- data.frame(
    x = factor(x, levels = lev),
    y = factor(y, levels = lev), 
    z)

  mat <- xtabs(dat$z ~ dat$x + dat$y)
  sym <- mat + t(mat)
  attr(sym, "class") <- NULL
  attr(sym, "call") <- NULL
  names(dimnames(sym)) <- NULL

  return(sym)
}

