#' Paste values together, replace NAs with blank, optional separator
#'
#' @param ... vectors or dataframe to be pasted together
#' @param sep separator between adjacent values in vectors
#' @param collapse separator between sets of values across vectors
#'
#' @return 
#' 
#' @examples
#' pasteVals(c(1,2,3,4,5), c(5,4,3,2,1), sep = "|")
#' pasteVals(c(1,2,3,4,5), c(5,4,3,2,1), c("a", "b", "c", "d", "e"), 
#'   sep = "|", collapse = "-")
#' pasteVals(data.frame(c(1,2,3,4,5), c(5,4,3,2,1)))
#' pasteVals(c(1,2,3,NA_real_,NA_real_), c(5,4,NA_real_,2,NA_real_))
#' 
#' @export
#' 
pasteVals <- function(..., sep = "", collapse = NULL) {
  ret <-
    apply(
      X=cbind(...),
      MARGIN=1,
      FUN=function(x) {
        if (all(is.na(x))) {
          NA_character_
        } else {
          paste(x[!is.na(x)], collapse = sep)
        }
      }
    )
  if (!is.null(collapse)) {
    paste(ret, collapse=collapse)
  } else {
    ret
  }
}


