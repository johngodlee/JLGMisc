#' Return the minimum bounding rectangle around a polygon
#'
#' @param x sf object containing only geometries of type \code{POLYGON} or 
#'     \code{MULTIPOLYGON}
#'
#' @return list containing vertex points (\code{ptx}), 
#'     width (\code{width}) height (\code{height}) and angle of bounding 
#'     rectangle of each polygon.
#' 
#' @examples
#' dat <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' min_box_list <- minBox(dat)
#' min_box_list[[1]]
#'
#' min_box_sf <- do.call(rbind, lapply(min_box_list, function(x) {
#'   pts_sf <- sf::st_as_sf(as.data.frame(x$pts), coords = c("X", "Y"))
#'   sf::st_sf(geometry = sf::st_convex_hull(sf::st_union(pts_sf)), crs = sf::st_crs(dat))
#'   }))
#' plot(sf::st_geometry(dat), col = NA, border = "blue")
#' plot(sf::st_geometry(min_box_sf), col = NA, border = "red", add = TRUE)
#' 
#' @importFrom sf st_is st_coordinates
#' 
#' @export
#' 
minBox <- function(x) {
  stopifnot(all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))))

  lapply(sf::st_geometry(x), function(y) {
    x_mat <- sf::st_coordinates(y)[,1:2]
    
    ## rotating calipers algorithm using the convex hull
    H <- chull(x_mat)  ## hull indices, vertices ordered clockwise
    n <- length(H)  ## number of hull vertices
    hull <- x_mat[H, ]  ## hull vertices
    
    ## unit basis vectors for all subspaces spanned by the hull edges
    hDir <- diff(rbind(hull, hull[1,]))  ## hull vertices are circular
    hLens <- sqrt(rowSums(hDir^2))  ## length of basis vectors
    huDir <- diag(1/hLens) %*% hDir  ## scaled to unit length
    
    ## unit basis vectors for the orthogonal subspaces
    ## rotation by 90 deg -> y' = x, x' = -y
    ouDir <- cbind(-huDir[,2], huDir[,1])
    
    ## project hull vertices on the subspaces spanned by the hull edges, and on
    ## the subspaces spanned by their orthogonal complements - in subspace coords
    projMat <- rbind(huDir, ouDir) %*% t(hull)
    
    ## range of projections and corresponding width/height of bounding rectangle
    rangeH <- matrix(numeric(n*2), ncol=2)  ## hull edge
    rangeO <- matrix(numeric(n*2), ncol=2)  ## orthogonal subspace
    widths <- numeric(n)
    heights <- numeric(n)
    
    for(i in seq(along=numeric(n))) {
      rangeH[i,] <- range(projMat[i,])
      
      ## the orthogonal subspace is in the 2nd half of the matrix
      rangeO[i,] <- range(projMat[n+i,])
      widths[i] <- abs(diff(rangeH[i,]))
      heights[i] <- abs(diff(rangeO[i,]))
    }
    
    ## extreme projections for min-area rect in subspace coordinates
    ## hull edge leading to minimum-area
    eMin <- which.min(widths*heights)
    hProj <- rbind(rangeH[eMin,], 0)
    oProj <- rbind(0, rangeO[eMin,])
    
    ## move projections to rectangle corners
    hPts <- sweep(hProj, 1, oProj[,1], "+")
    oPts <- sweep(hProj, 1, oProj[,2], "+")
    
    ## corners in standard coordinates, rows = x,y, columns = corners
    ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
    ## basis formed by hull edge and orthogonal subspace
    basis <- cbind(huDir[eMin,], ouDir[eMin,])
    hCorn <- basis %*% hPts
    oCorn <- basis %*% oPts
    pts <- t(cbind(hCorn, oCorn[,c(2,1)]))
    
    ## angle of longer edge pointing up
    dPts <- diff(pts)
    e <- dPts[which.max(rowSums(dPts^2)), ]  ## one of the longer edges
    eUp <- e * sign(e[2])  ## rotate upwards 180 deg if necessary
    deg <- atan2(eUp[2], eUp[1])*180/pi  ## angle in degrees
    
    return(list(pts = pts, width = heights[eMin], 
        height = widths[eMin], angle = deg))
  })
}

