#' Format a plus-minus number combo
#'
#' @param x atomic vector
#' @param y atomic vector
#' @param dx number of decimal places of x
#' @param dy number of decimal places of y
#' @param pm LaTeX plus-minus symbol
#' @param paren logical, should the measure of uncertainty be placed in 
#'     parentheses?
#'
#' @return character string
#' 
pmFormat <- function(x, y, dx = 2, dy = dx + 1, pm = "$\\pm$", paren = FALSE) {
  main <- numFormat(x, digits = dx)
  uncert <- numFormat(y, digits = dy)

  if (paren) {
    uncert <- paste0("(", uncert, ")")
  }

  return(paste0(main, pm, uncert))
}

