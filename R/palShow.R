#' Show a colour palette as a plot
#'
#' @param x vector of colour codes, either hex codes or string names
#'
#' @return Plot object
#'
#' @export
#'
palShow <- function(x) {
  image(1:length(x), 1, as.matrix(1:length(x)), 
    col = x, xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}

