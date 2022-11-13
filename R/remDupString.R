#' Remove duplicated characters within strings
#'
#' @param x vector of character strings
#'
#' @return vector of character strings with duplicated values removed 
#' 
#' @examples
#' remDupFlag(c("FU", "FUFF", "FJJ"))
#' 
#' 
#' @export
#' 
remDupSring <- function(x) {
  unlist(lapply(x, function(y) { 
    y <- unique(unlist(strsplit(y, split = "", fixed = FALSE, perl = TRUE)))
    if (all(!is.na(y))) { 
      paste(y, collapse = "")
    } else {
      y
    }
  }))
}
