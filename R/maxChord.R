#' Find the longest chord within a polygon
#'
#' @param x st_POLYGON object
#'
#' @return st_LINESTRING 
#' 
#' @examples
#' crd <- matrix(c(0, 0, 0.5, 1, 1, 0, 0, 1, 1.5, 1, 0, 0), ncol = 2)
#' poly <- st_polygon(list(crd))
#' maxChord(poly)
#' 
#' 
#' @export
#' 
maxChord <- function(x) {
  # Get polygon coordinates
  xy <- st_coordinates(x)[,1:2]

  # Compute distance matrix 
  m <- as.matrix(dist(xy))

  # Find largest element
  v <- which.max(m) - 1
  mij <- c(v %% nrow(m)+1, v %/% nrow(m)+1)

  # Create matrix defining longest chord
  chord <- rbind( 
    xy[mij[1],],
    xy[mij[2],]
  )

  # Convert to linestring
  chord_ls <- st_linestring(chord)

  # Return longest chord
  return(chord_ls)
}

#' Find the longest perpendicular chord within a polygon
#'
#' @param x st_POLYGON 
#'
#' @return st_LINESTRING
#' 
#' @examples
#' crd <- matrix(c(0, 0, 0.5, 1, 1, 0, 0, 1, 1.5, 1, 0, 0), ncol = 2)
#' poly <- st_polygon(list(crd))
#' maxPerpChord(poly)
#' maxPerpChord(poly, theta = pi/3)
#' 
#' @export
#' 
maxPerpChord <- function(x, chord = maxChord(x), theta = pi/2) { 

  # Convert linestring to coordinates 
  chord_xy <- st_coordinates(chord)[,1:2]

  # Compute length and angle of chord
  chord.length <- sqrt(diff(chord[,1])^2 + diff(chord[,2])^2)
  chord.theta <- atan2(diff(chord[,1]), diff(chord[,2]))

  # Perpendicular at angle plus pi/2 radians (90deg)
  perp <- chord.theta + theta

  # Get polygon vertices
  pts <- st_coordinates(x)[,c(1,2)]

  # Return perpendicular lines
  perplines <- lapply(seq_len(nrow(pts)), function(i) {
      # through the i-th vertex
      xy <- pts[i,, drop = FALSE]

      # Return line of given length through point at angle 
      perpline <- st_linestring( 
        cbind(
          xy[1] + c(chord.length,-chord.length) * sin(perp),
          xy[2] + c(chord.length,-chord.length) * cos(perp)
        )
      )

      # Intersect with polygon
      inters = st_intersection(x, perpline)

      # Return
      inters
  })

  # Get vector of intersection lengths, find the largest
  perplengths <- unlist(lapply(perplines, st_length))
  longest <- which.max(perplengths)
  perp_longest <- perplines[[longest]]
  
  # Return 
  return(perp_longest)
}
