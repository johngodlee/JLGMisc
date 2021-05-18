#' Get object sizes
#'
#' @param ... objects. If blank, all objects in \code{ls()} are used
#'
#' @export
#' 
objSize <- function() {
  sort(sapply(ls(), function(x) {
      object.size(get(x))
  }), decreasing = TRUE)
}

