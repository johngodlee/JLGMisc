#' Get KoboToolbox form IDs for a user
#'
#' @param server_url URL of the KoboToolbox server, 
#'     e.g. \url{https://kc.kobotoolbox.org/}
#' @param token optional KoboToolbox user access token
#' @param user optional KoboToolbox username
#' @param pass optional KoboToolbox password
#'
#' @return dataframe containing form IDs and names for all forms accessible to 
#'     the specified user
#' 
#' @details if \code{token} is supplied it is used preferentially over 
#'     \code{user} and \code{pass}. If token is not supplied, \code{user} and 
#'     \code{pass} must be supplied.
#'
#' @importFrom httr GET add_headers content authenticate
#' @importFrom readr read_csv
#' 
#' @export
#' 
koboFormID <- function(server_url, token = NULL, user = NULL, pass = NULL) {
  # Create URL
  kobo_url <- paste0(server_url, "api/v1/data.json")

  # Use either token or user+password
  if (!is.null(token)) {
    # Get data
    rawdata <- httr::GET(kobo_url, 
      httr::add_headers(Authorization = paste("Token", token)))
  } else {
    # Check, are both username and password supplied?
    if (any(is.null(user), is.null(pass))) {
      stop("If token not supplied, both user and pass must be supplied")
    }

    # Get data
    rawdata <- httr::GET(kobo_url, httr::authenticate(user, pass))
  }

  # Parse content
  dat <- httr::content(rawdata, "parsed")

  # Extract form strings and descriptions and put in dataframe
  out <- do.call(rbind, lapply(dat, function(x) {
    data.frame(id = x$id,
    id_string = x$id_string,
    title = x$title,
    description = x$description,
    url = x$url)
  }))

  # Return
  return(out)
}



