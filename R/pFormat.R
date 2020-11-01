#' Format p-values for use in LaTeX
#'
#' @param p numeric vector of p-values
#' @param lev vector of threshold values 
#' @param round logical, if TRUE values not lower than any critical threshold 
#'     are rounded to \code{n} decimal places  
#' @param digits number of decimal places to round p values to if not lower than 
#'     any critical threshold
#' @param asterisks, logical, alternatively return asterisks at thresholds
#'
#' @return character vector 
#' 
#' @examples
#' pFormat(0.03)
#' pFormat(c(0.06,0.05,0.005))
#' @export
#' 
pFormat <- function(p, lev = c(0.001, 0.01, 0.05), round = TRUE, digits = 2,
  asterisks = FALSE) {
  if (!all(is.numeric(lev))) {
    stop("lev must be numeric")
  }
  if (!all(is.numeric(p))) {
    stop("p must be numeric")
  }

  lev <- sort(lev, decreasing = TRUE)

  out <- list()
  for (i in seq(length(p))) {
    if (any(p[i] < lev)) {
      thresh <- max(which(p[i] < lev))
      if (asterisks) {
        out[i] <- paste0(rep("*", times = thresh), collapse = "")
      } else {
        out[i] <- paste0("p<", lev[thresh])
      }
    } else {
      if (asterisks) {
        out[i] <- "-"
      } else {
        if (round) {
          out[i] <- as.character(round(p[i], digits))
        } else {
          out[i] <- as.character(p[i])
        }
      }
    }
  }
  return(unlist(out))
}
