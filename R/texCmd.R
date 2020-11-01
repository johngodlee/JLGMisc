#' Generate a LaTeX command from R object
#'
#' @param stat atomic vector
#' @param func character string with name of LaTeX function
#'
#' @return character string
#' 
#' @examples
#' a <- 3.145
#' texCmd(a, "pi")
#' @export
#' 
texCmd <- function(stat, func){
  paste0("\\newcommand{\\", func, "}{", stat, "}")
}

