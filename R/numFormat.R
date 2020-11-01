#' Format a number for LaTeX
#'
#' @param x atomic vector
#' @param digits number of digits 
#' @param method method of rounding, either "round" or "signif"
#'
#' @return character vector
#' @export
#' 
numFormat <- function(x, digits = 2, method = "round"){
  sprintf(paste0("%.",digits,"f"),
    if (method == "round") {
      round(x, digits = digits)
    } 
    else if (method == "signif") {
      signif(x, digits = digits)
    })
}

