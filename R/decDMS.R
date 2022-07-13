#' Convert decimal degrees to degrees, minutes, seconds
#'
#' @param dec vector of decimal degrees
#' @param dire one of "lat" (latitude) or "lon" (longitude) or NULL
#' @param dataframe logical, should a dataframe be returned instead of a 
#'     formatted string
#'
#' @return character vector of coordinates formatted as degrees, minutes and seconds
#' 
#' @examples 
#' x <- c(15.53, -15.54, 0.1)
#' decDMS(x)
#' decDMS(x, "lon")
#' decDMS(x, "lat")
#' decDMS(x, "lat", dataframe = TRUE)
#' 
#' @export
#' 
decDMS <- function(dec, dire = NULL, dataframe = FALSE) {

  decabs <- abs(dec)

  degr <- floor(abs(decabs))
  deg <- (abs(decabs)-degr) * 60
  minu <- floor(deg)
  seco <- formatC(round((deg-minu) * 60, 2), 2, format = "f")
  if (is.null(dire)) {
    dire <- ""
  } else if (dire == "lat") {
    dire <- ifelse(dec > 0, " N", " S")
  } else if (dire == "lon") {
    dire <- ifelse(dec > 0, " E", " W")
  } else {
    stop("dire must be 'lat', 'lon', or NULL")
  }

  if (dataframe) { 
    out <- data.frame(degr, minu, seco)
    if (!is.null(dire)) {
      out$dire <- dire
    }
  } else {
    out <- paste0(degr, "°", minu, "'", seco, "''", dire)
  } 

  return(out)
}

