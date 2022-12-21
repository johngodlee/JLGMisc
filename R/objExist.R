#' Check if an object exists using the object itself, rather than char string
#'
#' @param ... any number of objects
#'
#' @return logical vector 
#' 
#' @examples
#' x <- "test"
#' objExist(x)
#' objExist(x, y)
#' 
#' @export
#' 
objExist <- function(...) {
    vars <- as.character(substitute(...()))
    sapply(vars, exists)
}

