#' In a vector of species genera, expand sequential abbreviations
#'
#' @param x vector of species genera strings
#' @param abbrev character used to signify start of abbreviation
#'
#' @return character vector of expanded species genera
#' 
#' @examples
#' vec <- c("Ochna", "O.", "Ochna", "Brachystegia", "B.", "B.")
#' fillGenus(vec)
#' @export
#' 
fillGenus <- function(x, abbrev = "."){
	rel_enc <- rle(as.character(x))
	empty <- which(grepl("\\.", rel_enc$value))
	rel_enc$values[empty] <- rel_enc$value[empty-1] 
	inverse.rle(rel_enc)
}

