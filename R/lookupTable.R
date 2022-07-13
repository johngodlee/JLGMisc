#' Use a lookup table to match values 
#'
#' @param x vector of original values
#' @param y dataframe lookup table
#' @param old column name string of original values
#' @param new column name string of new values
#'
#' @return vector of updated values
#' 
#' @examples
#' x <- c("A", "B", "B", "C", "A")
#' lookup <- data.frame(old = c("A", "B", "C", "D"), new = c(1, 2, 3, 4))
#' lookupTable(x, lookup, "old", "new")
#' 
#' @export
#' 
lookupTable <- function(x, y, old, new) {
  lookup[[new]][match(x, lookup[[old]])]
}


