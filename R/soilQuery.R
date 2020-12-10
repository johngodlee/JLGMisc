#' Query SoilGrids via the REST API 
#' 
#' See https://rest.soilgrids.org/soilgrids/v2.0/docs for more information
#' 
#' @param x dataframe of plot level data
#' @param plot_id column name string of plot IDs
#' @param longitude_of_centre column name string of plot longitude
#' @param latitude_of_centre column name string of plot latitude
#' @param attrib vector of soil attributes to extract
#' @param depth vector of soil depths over which to extract the attributes, e.g. 0-5, 30-60
#' @param average optional list of vectors, each of length 2, describing ranges 
#'     of depths over which to average (mean) the values of attrib
#' @param value vector of soil values to extract for each attribute, e.g. mean, Q0.05 
#' 
#' @details See \code{soilQueryOptions()} for all possibly combinations of 
#'     attrib, depth, and value
#'
#' @return dataframe of soil values by depth
#' 
#' @importFrom httr GET content
#' @export
#' 
soilQuery <- function(x, plot_id = "plot_id", 
  longitude_of_centre = "longitude_of_centre", 
  latitude_of_centre = "latitude_of_centre", 
  attrib = c("cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", 
    "phh20", "sand", "silt", "soc"),
  depth = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"), 
  average = list(c(0,30)), 
  value = "mean") {

  default_attrib <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", 
    "phh20", "sand", "silt", "soc")
  default_depth <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
  depth_values <- unique(unlist(strsplit(default_depth, "-")))
  default_value <- c("Q0.05", "Q0.5", "Q0.95", "mean", "uncertainty")

  # Check input
  if (any(!attrib %in% default_attrib)) {
    stop("Illegal soil attribute: ",
      paste(attrib[!attrib %in% default_attrib], collapse = ", "),
      "\n\tAllowed values: ", 
        paste(default_attrib, collapse = ", "),
      "\n\tRun `soilQueryOptions()` to find valid attributes")
  }
  if (any(!depth %in% default_depth)) {
    stop("Illegal depth: ",
      paste(depth[!depth %in% default_depth], collapse = ", "),
      "\n\tallowed values: ", 
      paste(default_depth, collapse = ", "),
      "\n\tRun `soilQueryOptions()` to find valid depths")
  }
  if (!is.null(average)) {
    if (any(!unlist(average) %in% depth_values)) {
    stop("Illegal averaging depth: ",
        paste(average[!unlist(average) %in% depth_values], collapse = ", "),
        "\n\tallowed values: ", 
        paste(depth_values, collapse = ", "))
    }
    if (any(unlist(average) > max(depth_values) | unlist(average) < min(depth_values))) {
      stop("average range values outside sampled depths: ",
        paste(unlist(average)[unlist(average) > max(depth_values) | unlist(average) < min(depth_values)], 
          collapse = ", "))
    }
    if (any(sapply(average, function(y) {
          length(y) != 2
        }))) {
      stop("All average ranges must be vectors of length 2")
    }
  }
  if (any(!value %in% default_value)) {
    stop("Illegal value: ",
        paste(value[!value %in% default_value], collapse = ", "),
        "\n\tallowed values: ", 
        paste(default_value, collapse = ", "),
      "\n\tRun `soilQueryOptions()` to find valid values")
  }

  # Construct query
  attrib_string <- paste0(paste0("&property=", attrib), collapse = "")
  depth_string <- paste0(paste0("&depth=", depth, "cm"), collapse = "")
  value_string <- paste0(paste0("&value=", value), collapse = "")

  queries <- lapply(1:nrow(x), function(y) {
    call <- paste0("https://rest.soilgrids.org/soilgrids/v2.0/properties/query?", 
      "lon=", x[y, longitude_of_centre], 
      "&lat=", x[y, latitude_of_centre], 
      attrib_string, 
      depth_string,
      value_string)
  })
  
  # Run GET query
  query_get <- lapply(queries, httr::GET)

  # If any queries fail, end function
  if (any(unlist(lapply(query_get, `[[`, "status_code")) != 200)) {
    stop("Some queries failed")
  }

  # Flatten to list
  query_list <- lapply(query_get, httr::content, as = "parsed")

  # For each query, for each attrib, for each depth, for each value, extract values
  query_extract <- lapply(query_list, function(y) {
    lapply(y$properties$layers, function(z) {
      lapply(z$depths, function(v) {
        lapply(v$values, function(w) {
          w 
        })
      })
    })
  })

  # Make tidy dataframe of values
  extract_df <- data.frame(
    plot_id = rep(x[[plot_id]], each = length(attrib) * length(depth) * length(value)),
    attrib =  rep(rep(attrib, each = length(depth) * length(value)), times = length(x[[plot_id]])),
    depth =   rep(rep(rep(depth, each = length(value)), times = length(x[[plot_id]])), times = length(attrib)),
    value =   rep(rep(rep(value, times = length(x[[plot_id]])), times = length(attrib)), times = length(depth)),
    extract = unlist(query_extract))

  # Optionally average each value and attrib over depths
  if (!is.null(average)) {
    extract_split <- split(extract_df, 
      list(extract_df$plot_id, extract_df$attrib, extract_df$value))

    extract_df <- do.call(rbind, lapply(extract_split, function(y) {
      do.call(rbind, lapply(average, function(z) {
        y$min_depth <- sapply(strsplit(y$depth, "-"), `[`, 1)
        y$max_depth <- sapply(strsplit(y$depth, "-"), `[`, 2)
        out_df <- data.frame(plot_id = y[1,plot_id],
          attrib = y[1, "attrib"],
          depth = paste(y$min_depth[which(y$min_depth == z[1])], y$max_depth[which(y$max_depth == z[2])], sep = "-"),
          value = y[1, "value"],
          extract = mean(y$extract[which(y$min_depth == z[1]) : which(y$max_depth == z[2])],
          na.rm = TRUE))
      }))
    }))
    row.names(extract_df) <- NULL
  }

  return(extract_df)
}
