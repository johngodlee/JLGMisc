#' Find local peaks in a distribution
#'
#' @param x vector of value series
#' @param m number of adjacent points needed to be lower to count as a peak
#'
#' @return vector of positions in x where there is a peak
#' 
#' @details use findPeaks(-x) to find troughs
#' 
#' @examples
#' x <- w <- rnorm(n = 100, mean = 0, sd = 1) 
#' for (t in 2:100) { x[t] <- x[t - 1] + w[t] }
#' p3 <- findPeaks(x)
#' p10 <- findPeaks(x, 10)
#' t10 <- findPeaks(-x, 10)
#' plot(x, type = "l")
#' abline(v = p10, col = "green", lwd = 5)
#' abline(v = p3, col = "blue")
#' abline(v = t10, col = "red")
#' 
#' @export
findPeaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))

  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) {
      return(i + 1) 
    } else { 
      return(numeric(0))
    }
  })

  pks <- unlist(pks)
  return(pks)
}

