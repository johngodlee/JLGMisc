#' Convert degrees, minutes, seconds to decimal degrees
#'
#' @param degr vector of degrees
#' @param minu vector of minutes
#' @param seco vector of seconds
#' @param dire direction, e.g. N/S, E/W
#'
#' @return atomic vector of decimal degrees
#' 
#' @examples
#' x <- "15-05-23-S"
#' dms_split <- strsplit(x, "-")[[1]]
#' dmsDeg(
#'   as.numeric(dms_split[1]), 
#'   as.numeric(dms_split[2]), 
#'   as.numeric(dms_split[3]), 
#'   dms_split[4])
#' 
#' @export
#' 
dmsDeg <- function(degr, minu, seco, dire) {
  out <- degr + (minu / 60) + (seco / 3600)
  out <- ifelse(dire %in% c("W", "S"), -out, out)
  return(out)
}
