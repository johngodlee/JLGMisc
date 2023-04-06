#' Convert a raster to a dataframe for plotting with ggplot2
#'
#' @param x raster object
#'
#' @return dataframe object
#' 
#' @export
#' 
rastFort <- function(x) {
  x_spdf <- as(x, "SpatialPixelsDataFrame")
  x_df <- as.data.frame(x_spdf)
  return(x_df)
}

