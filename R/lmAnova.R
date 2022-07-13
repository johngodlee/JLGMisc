#' Run an ANOVA on a list of linear models
#'
#' @param mod_list list of \code{lm()} model objects 
#'
#' @export
#' 
lmAnova <- function(mod_list) {
  list_name <- deparse(substitute(mod_list))
  eval(parse(text=paste("anova(",paste(list_name, "[[",1:length(mod_list),"]]",sep="",collapse=","),")")))
}

