#' Generate regular 2D shapes by defining the number of sides and the radius
#'
#' @param nsides number of sides/vertices
#' @param radius radius of shape
#' @param centre numeric vector of length 2, defining the x,y coordinates of the shape centre
#' @param angle starting angle for first vertex
#'
#' @return dataframe of vertices bounding the shape
#' 
#' @examples
#' polyVert(5, 10)
#' polyVert(4, 5, centre = c(10, 1), angle = 45)
#' 
#' @export
#' 

polyVert <- function(nsides, radius, centre = c(0, 0), angle = 0) {
  steps <- 2 * pi / nsides
  x <- NULL
  y <- NULL

  for (i in seq_len(nsides)) {
      x[i] <- radius * cos(angle);
      y[i] <- radius * sin(angle);
      angle <- angle + steps;
  }

  x <- x + centre[1]
  y <- y + centre[2]

  return(data.frame(x,y))
}

