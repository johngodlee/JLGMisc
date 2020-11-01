#' Generate a species by site abundance matrix
#'
#' @param x dataframe of individual records
#' @param site_id column name string of site IDs
#' @param species_id column name string of species names
#' @param fpc optional column name string of sampling weights of each record, 
#'     between 0 and 1 
#' @param abundance optional column name string with an alternative abundance 
#'     measure such as biomass, canopy cover, body length
#'
#' @return dataframe of species abundances (columns) per site (rows)
#' 
#' @examples
#' x <- data.frame(site_id = rep(c("A", "B", "C"), each = 3), 
#'   species_id = sample(c("a", "b", "c", "d"), 9, replace = TRUE), 
#'   fpc = rep(c(0.5, 0.6, 1), each = 3), 
#'   abundance = seq(1:9))
#' abMat(x, "site_id", "species_id")
#' abMat(x, "site_id", "species_id", "fpc")
#' abMat(x, "site_id", "species_id", "fpc", "abundance")
#' 
#' @export
#' 
abMat <- function(x, site_id, species_id, fpc = NULL, abundance = NULL) {
  # If no fpc or abundance, make 1
  if (is.null(fpc)) {
    x$fpc <- 1
  } else {
  	x$fpc <- x[[fpc]]
  }
  if (is.null(abundance)) {
    x$abundance <- 1 
  } else {
  	x$abundance <- x[[abundance]]
  }

  # Get all species and sites
  species <- unique(x[[species_id]])
  sites <- unique(x[[site_id]])

  # Create empty species by site matrix
  comm <- matrix(0, nrow = length(sites), ncol = length(species))

  # Fill matrix
  for (i in seq(length(sites))) {
    for(j in seq(length(species))) {
      abu <- x[x[[site_id]] == sites[i] & x[[species_id]] == species[j], 
        c(site_id, species_id, "fpc", "abundance")]
      comm[i,j] <- sum(1 * abu$abundance / abu$fpc, na.rm = TRUE)
    }
  }

  # Make tidy with names
  comm <- data.frame(comm)
  names(comm) <- species
  row.names(comm) <- sites

  return(comm)
}
