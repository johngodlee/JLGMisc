#' Dynamically install packages as needed and load them
#'
#' @param ... packages as character strings
#'
#' @examples 
#' usingPkg("dplyr", "tidyr", "ggplot2")
#' @export
#' 
usingPkg <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  if(length(need) > 0) { 
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}
