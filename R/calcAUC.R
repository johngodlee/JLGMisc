#' Calculate Area Under Curve (AUC) using trapesium approximation
#'
#' @param x vector of x values
#' @param y vector of y values
#' @export
#'
calcAUC <- function(x, y) {
  sum(diff(x) * (head(y,-1)+tail(y,-1)))/2
}

