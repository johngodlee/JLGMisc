#' Find the closest named colour to a given six character hex colour
#'
#' @param x vector of six digit hex colours
#' @param method colour space, either "rgb" or "hsv".
#' @param metric distance metric, either "euclidean" or "manhattan".
#'
#' @return vector of nearest named colour
#' 
#' @details If metric is "euclidean", distances are root sum-of-squares 
#'     differences. "manhattan" distances are the sum of absolute differences.
#' 
#' @examples
#' hexName(c("#117733", "#b58900", "#855C75"))
#' 
#' @export
#' 
hexName <- function(x, method = "rgb", metric = "euclidean") {
  # Check input is valid
  if (any(nchar(x) != 7) | any(!grepl("^#", x))) {
    stop("Hex code(s) invalid")
  }

  if (!method %in% c("rgb", "hsv")) {
    stop("method must be 'rgb' or 'hsv'")
  }

  if (!metric %in% c("euclidean", "manhattan")) {
    stop("metric must be 'euclidean' or 'manhattan'")
  }

  # Convert hex string to RGB 
  x <- col2rgb(x)

  # Create matrix of named colours as RGB values
  coltab <- col2rgb(colors())

  # If HSV
  if (method == "hsv") {
    x <- rgb2hsv(x)
    coltab <- rgb2hsv(coltab)
  }

  # Find nearest named colour by metric
  if (metric == "euclidean") {
    out <- colors()[apply(x, 2, function(y) {
      which.min(apply(apply(coltab, 2, "-", y)^2, 2, sum)) 
    })]
  } else if (metric == "manhattan") {
    out <- colors()[apply(x, 2, function(y) {
      which.min(apply(abs(apply(coltab, 2, "-", y)), 2, sum))
    })]
  }

  # Return
  return(out)
}
