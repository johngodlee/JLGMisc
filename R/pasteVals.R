#' Paste values together, replace NAs with blank, optional separator
#'
#' @param ... vectors or dataframe to be pasted together
#' @param sep separator between adjacent values in vectors
#' @param collapse separator between sets of values across vectors
#' @param unique logical, if TRUE duplicated values are removed
#' @param sort logical, if TRUE values are sorted
#'
#' @return vector of pasted values
#' 
#' @examples
#' pasteVals(c(1,2,3,4,5), c(5,4,3,2,1), sep = "|")
#' pasteVals(c(1,2,3,4,5), c(5,4,3,2,1), c("a", "b", "c", "d", "e"), 
#'   sep = "|", collapse = "-")
#' pasteVals(data.frame(c(1,2,3,4,5), c(5,4,3,2,1)))
#' pasteVals(c(1,2,3,NA_real_,NA_real_), c(5,4,NA_real_,2,NA_real_))
#' pasteVals(c(6,2,3,4,5,6), c(6,5,4,4,2,1), unique = TRUE)
#' pasteVals(c(6,2,3,4,5,6), c(6,5,4,4,NA_real_,1), sort = TRUE, remna = FALSE)
#' pasteVals(c(6,2,3,4,5,6), c(6,5,4,4,NA_real_,1), sort = TRUE)
#' 
#' @export
#' 
pasteVals <- function(..., sep = "", collapse = NULL, 
  remna = TRUE, unique = FALSE, sort = FALSE) {
  ret <-
    apply(
      X = cbind(...),
      MARGIN = 1,
      FUN = function(x) {
        if (all(is.na(x))) {
          NA_character_
        } else {
          if (sort) {
            x <- sort(x, na.last = TRUE)
          }
          if (remna) {
            x <- x[!is.na(x)]
          }
          if (unique) {
            x <- x[!duplicated(x)]
          }
          paste(x, collapse = sep)
        }
      }
    )
  if (!is.null(collapse)) {
    paste(ret, collapse = collapse)
  } else {
    ret
  }
}


