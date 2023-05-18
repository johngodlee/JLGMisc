#' Get KoboToolbox form data as a dataframe for a given form
#'
#' @param server_url URL of the KoboToolbox server, 
#'     e.g. \url{https://kc.kobotoolbox.org/}
#' @param formid ID or ID string for a given KoboToolbox form
#' @param token optional KoboToolbox user access token
#' @param user optional KoboToolbox username
#' @param pass optional KoboToolbox password
#'
#' @return dataframe containing submitted data for the specified form
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
koboDataGet <- function(server_url, formid, 
  token = NULL, user = NULL, pass = NULL) {

  # Create URL 
  kobo_csv_url <- paste0(server_url, "api/v1/data/", formid, ".csv")

  # Use either token or user+password
  if (!is.null(token)) {
    # Get data
    rawdata <- httr::GET(kobo_csv_url, 
      httr::add_headers(Authorization = paste("Token", token)))
  } else {
    # Check, are both username and password supplied?
    if (any(is.null(user), is.null(pass))) {
      stop("If token not supplied, both user and pass must be supplied")
    }

    # Get data
    rawdata <- httr::GET(kobo_csv_url, httr::authenticate(user, pass))
  }

  # Create dataframe from data
  out <- readr::read_csv(httr::content(rawdata, "raw", encoding = "UTF-8"),
    na = c("", "NA", "n/a"))

  # Return
  return(out)
}

