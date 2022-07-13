#' Format p-values for use in LaTeX
#'
#' @param p numeric vector of p-values
#' @param lev vector of threshold values 
#' @param asterisks, logical, alternatively return asterisks at thresholds
#' @param ps, logical, if TRUE return 
#' @param digits optional, number of decimal places to round p values to if 
#'     not lower than any critical threshold
#'
#' @return character vector 
#' 
#' @examples
#' pFormat(0.03)
#' pFormat(c(0.06,0.05,0.005))
#' pFormat(0.02, ps = FALSE)
#' pFormat(0.02, asterisks = TRUE)
#' pFormat(0.55, digits = 1)
#' @export
#' 
pFormat <- function(p, lev = c(0.001, 0.01, 0.05), asterisks = FALSE, 
  ps = TRUE, digits = NULL) {
  if (!all(is.numeric(lev))) {
    stop("lev must be numeric")
  }
  if (!all(is.numeric(p))) {
    stop("p must be numeric")
  }

  lev <- sort(lev, decreasing = TRUE)

  out <- list()
  for (i in seq_along(p)) {
    if (is.na(p[i])) {
      out[i] <- NA_character_
    } else if (any(p[i] < lev)) {
      thresh <- max(which(p[i] < lev))
      if (asterisks) {
        out[i] <- paste0(rep("*", times = thresh), collapse = "")
      } else {
        out[i] <- paste0(ifelse(ps, "p", ""), "<", lev[thresh])
      }
    } else {
      if (asterisks) {
        out[i] <- "-"
      } else {
        if (!is.null(digits)) {
          out[i] <- paste0(ifelse(ps, "p=", ""), as.character(round(p[i], digits)))
        } else {
          out[i] <- paste0(ifelse(ps, "p=", ""), as.character(p[i]))
        }
      }
    }
  }
  return(unlist(out))
}

