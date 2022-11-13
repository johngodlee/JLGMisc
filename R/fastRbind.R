#' Fast rbind for dataframes
#'
#' @param x list of dataframe objects
#'
#' @return dataframe
#' 
#' @export
#' 
fastRbind <- function(x) { 
  list2DF(lapply(setNames( seq_along(x[[1]]), names(x[[1]])), 
      function(i) {
        unlist(lapply(x, `[[`, i), FALSE, FALSE)
      }))
}

