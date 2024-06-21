#' Add a circular buffer to a point until a valid raster value is extracted
#'
#' @param x raster layer
#' @param y sf points object 
#' @param b optional, inital buffer radius, or vector of buffer radii in metres
#' @param bstep optional, incremental buffer radius increase in metres
#' @param ... additional arguments passed to `terra::extract()`
#'
#' @return A dataframe as returned by `terra::extract()` with an additional
#'     column `b`, which specifies the radius of the buffer necessary to 
#'     intersect a valid raster pixel.
#'
#' @examples
#' r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
#' values(r) <- seq(0, 100, length.out = 100*100)
#' p <- data.frame(
#'   id = LETTERS[1:10],
#'   lon = seq(-0.5, 1.5, length.out = 10),
#'   lat = seq(-0.5, 1.5, length.out = 10))
#' psf <- st_as_sf(p, coords = c("lon", "lat"), crs = 4326)
#' crs(r) <- crs(psf)
#' 
#' nearExtract(r, psf)
#' nearExtract(r, psf, b = 1000)
#' nearExtract(r, psf, b = 1000, bstep = 10000)
#' nearExtract(r, psf, bstep = 10000)
#' nearExtract(r, psf, fun = min)
#' nearExtract(r, psf, fun = min, method="bilinear")
#'
#' @importFrom terra cellSize global extract
#' @importFrom sf st_buffer
#' 
nearExtract <- function(x, y, fun = mean, b = NULL, bstep = NULL, ...) { 

  # Function must be specified
  if (!is.function(fun)) { 
    stop("fun must be a valid function")
  }

  # bstep ignored if length(b) > 1
  if (length(b) > 1 & !is.null(bstep)) { 
    warning("length(b) > 1, bstep ignored")
  }

  # If b is a vector
  if (length(b) > 1) {
    # If buffer is a vector
    b <- sort(b)
    b1 <- b[1]
    bstep <- NULL
  } else if (is.null(b)) { 
    # If b is not specified, start with radius equal to raster mean cell size
    b1 <- unlist(sqrt(terra::global(terra::cellSize(x, unit = "km"), "mean") / pi))
  } else { 
    b1 <- b
  }

  # If bstep not specified, use the raster mean cell size
  if (is.null(bstep)) { 
    bstep <- b1
  }

  # Attempt to extract
  val <- terra::extract(x, y, fun = fun, na.rm = TRUE, ...)
  val$b <- NA_real_

  # If extraction failed for any individual
  if (any(is.na(val[,-ncol(val)]))) {
    # Set buffer to initial value
    bi <- b1

    # Sequentially increase buffer diameter until all NA filled
    while (any(is.na(val[,-ncol(val)])) & 
      (if (length(b) > 1) { bi != max(b) } else { TRUE } )) {
      # See progress
      message(bi)

      # Find missing values
      val_fill <- which(apply(val[,-ncol(val)], 1, function(i) { any(is.na(i)) }))

      # Apply buffer to values
      yb <- sf::st_buffer(y[val_fill,], bi)

      # Attempt to extract
      val_new <- terra::extract(x, yb, fun = fun, na.rm = TRUE, ...)

      # Add buffer size to successfully filled values
      val_new$b <- bi
      val_new$b[is.na(val_new[,2])] <- NA_real_

      # Fill missing values 
      val[val_fill,] <- val_new

      # Increase buffer size
      if (length(b) > 1) { 
        # If b is a vector,
        # move to the next largest buffer size
        bi <- b[which(b == bi) + 1]
      } else {
        # Otherwise add bstep
        bi <- bi + bstep
      }
    }
  }

  # Return
  return(val)
}

