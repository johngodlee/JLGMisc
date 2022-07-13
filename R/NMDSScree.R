#' Create scree plot for NMDS
#'
#' Runs \code{vegan::metaMDS()} across a specified range of dimensions
#' 
#' @param x species by site abundance matrix
#' @param dims either an integer of the maximum number of dimensions or an 
#'     integer vector of dimensions to try
#' @param ... additional arguments passed to \code{vegan::metaMDS()}
#'
#' @return plot object
#' @export
#' 
NMDSScree <- function(x, dims = 10, ...) {
  # Create dimensions vector
  if (length(dims) == 1) {
    dim_vec <- seq(dims) 
  } else {
    dim_vec <- dims
  }

  # Create list of metaMDS objects
  meta_list <- lapply(dim_vec, function(y) {
    metaMDS(x, autotransform = F, k = y, ...)
  })

  # Extract stress values
  stress_vec <- unname(unlist(lapply(meta_list, `[`, "stress")))

  # Create plot
  plot(dim_vec, stress_vec,
    xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
}

