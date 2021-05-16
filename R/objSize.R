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

function() {sort(sapply(ls(), function(x) format(object.size(get(x)), unit = 'auto')))}


objSize()
