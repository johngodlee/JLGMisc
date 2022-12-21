#' Find the closest hex triplet to a given six character hex colour
#'
#' @param x vector of six digit hex colours
#'
#' @return 
#' 
#' @examples
#' hexTrip(c("#117733", "#b58900", "#855C75"))
#' hexTrip("#fffffff")
#' hexTrip("855C75")
#' 
#' @export
#' 
hexTrip <- function(x) {
  if (any(nchar(x) != 7) | any(!grepl("^#", x))) {
    stop ("Hex code(s) invalid")
  }

  unlist(lapply(x, function(y) {
    paste(c("#", sprintf("%x", (col2rgb(y) + 8) %/% 17)), collapse = "")
  }))
}


