#' Format warnings, messages and errors
#'
#' @param x vector of row positions, IDs etc. to display
#' @param warn character string with warning, message or error
#' @param type choose either "warning", "message" or "error" 
#' @param n integer, number of items from x to return in warning
#' 
#' @export
#' 
warnFormat <- function(x, warn, type, n = 10, sep = ",") {
  if (length(x) == 0) {
    return(invisible())
  }

  nlen <- ifelse(length(x) > n, n, length(x))
  endv <- ifelse(length(x) > n, " ...", "")
  warn <- paste0(trimws(warn), " ")

  if (length(type) != 1 | !type %in% c("warning", "message", "error")) {
    stop("warn not 'warning', 'message' or 'error'")
  }

  if (length(x) > 0) {
    if (type == "warning") {
      warning(warn, "\n", paste(x[1:nlen], collapse = sep), endv, 
        call. = FALSE)
    } else if (type == "message") {
      message(warn, "\n", paste(x[1:nlen], collapse = sep), endv)
    } else if (type == "error") {
      stop(warn, "\n", paste(x[1:nlen], collapse = sep), endv, call. = FALSE)
    }
  }
}

