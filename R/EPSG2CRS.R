#' Get a valid CRS object from an EPSG code.
#'
#' Convert an EPSG code to proj4string or WKT (Well-Known Text) 
#'
#' @param x vector of EPSG codes.
#'     See http://www.epsg-registry.org/ or http://spatialreference.org/.
#' @param wkt logical, if true returns sf WKT CRS object rather than proj4string
#'
#' @return A vector of CRS objects or list of WKT objects
#'
#' @importFrom sf st_crs
#' @export
#'
EPSG2CRS <- function(x, wkt = FALSE){

  # Convert EPSG code to integer
  epsg <- as.integer(x)

  crs <- lapply(epsg, function(y) {
    sf::st_crs(y)
  })

  if (!wkt) {
    crs <- unlist(lapply(crs, function(y) {
        y$proj4string
    }))
  }

  return(crs)
}

