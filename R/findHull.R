#' Find the convex hull of a 2D set of points
#'
#' @param dat dataframe of points
#' @param x column name string of x axis values
#' @param y column name string of y axis values
#' @param group optional column name string of grouping variable
#'
#' @return dataframe of convex hulls as vertices
#' 
#' @examples 
#' dat <- data.frame(x = rnorm(50), y = rnorm(50), 
#'   group = sample(seq_len(5), 50, replace = TRUE))
#'
#' findHull(dat, "x", "y", "group")
#' 
#' @export
#' 
findHull <- function(dat, x, y, group = NULL) {
  if (is.null(group)) {
    out <- dat[chull(dat[[x]], dat[[y]]), c(x, y)]

  } else {
    hulls <- by(dat, dat[[group]], function(i) {
      i[chull(i[[x]], i[[y]]), c(x, y, group)]
    })

    out <- do.call(rbind, hulls)
  }

  return(out)
}
