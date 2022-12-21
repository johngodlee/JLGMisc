#' Calculate end points of line given start point, slope, and distance
#'
#' @param x x coordinate of start point
#' @param y y coordinate of start point
#' @param m slope
#' @param d distance
#'
#' @return dataframe of coordinates for both endpoints
#' 
#' @export
#' 
endPoints <- function(x, y, m, d) {
  k <- d / sqrt(1 + (m^2))
  xmin <- x - k
  xmax <- x + k
  ymin <- y - (m * k)
  ymax <- y + (m * k)

  return(data.frame(xmin, ymin, xmax, ymax))
}

