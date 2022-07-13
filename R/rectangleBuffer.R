#' Create a rectangular buffer around an sfc point object
#'
#' @param x sfc containing points
#' @param x_len length of rectangle along x axis
#' @param y_len length of rectangle along y axis
#' @param deg angle in degrees of rotation
#'
#' @return sf polygon object
#' @import sf 
#' 
#' @export
#' 
rectangleBuffer <- function(x, x_len, y_len, deg = 0) { 
  # Define rotation in radians
  rads <- deg * 0.0174532925

  # For each row in pts
  recs <- sf::st_sfc(do.call(rbind, lapply(seq_len(nrow(x)), function(i) {

    # Get coordinates
    pts <- sf::st_coordinates(x[i,])

    # Create empty matrix
    pts_df <- as.data.frame(matrix(nrow = 4, ncol = 2))
    colnames(pts_df) <- c("x", "y")

    # Set points
    pts_df[1,1] <- (pts[1] - x_len / 2) 
    pts_df[1,2] <- (pts[2] + y_len / 2)

    pts_df[2,1] <- (pts[1] + x_len / 2) 
    pts_df[2,2] <- (pts[2] + y_len / 2)

    pts_df[3,1] <- (pts[1] - x_len / 2) 
    pts_df[3,2] <- (pts[2] - y_len / 2) 

    pts_df[4,1] <- (pts[1] + x_len / 2) 
    pts_df[4,2] <- (pts[2] - y_len / 2) 

    # Convert to sf
    pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"), crs = sf::st_crs(x))

    # Create convex hull of points
    rec_sf <- sf::st_convex_hull(sf::st_union(pts_sf))

    # Create centroid of rectangle
    rec_cnt <- sf::st_centroid(rec_sf)

    # Rotate polygon
    rot_sf <- (rec_sf - rec_cnt) * rot(rads) + rec_cnt

    # Return
    return(rot_sf)
  })))

  return(recs)
}

