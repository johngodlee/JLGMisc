#' Show a colour palette as a plot
#'
#' @param x vector of colour codes, either hex codes or string identifiers
#'
#' @return Plot object
#'
#' @export
#'
palShow <- function(x){
  vec <- rep(1, times = length(x))

  p <- barplot(vec, 
    names.arg = x, 
    axes = FALSE,
    col = x)

  return(p)
}
