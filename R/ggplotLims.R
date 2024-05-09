#' Get axis limits from a ggplot2 plot object
#'
#' @param x ggplot2 plot object
#'
#' @return named vector of plot axis limits
#' 
#' @examples
#' library(ggplot2)
#' dat <- data.frame(
#'   x = rnorm(1000),
#'   y = rnorm(1000))
#' p <- ggplot(dat, aes(x = x, y = y )) +
#'   geom_point()
#' ggplotLims(p)
#' 
#' @importFrom ggplot2 ggplot_build
#' 
#' @export
#' 
ggplotLims <- function(x) {
    gb = ggplot2::ggplot_build(x)
    xmin = gb$layout$panel_params[[1]]$x.range[1]
    xmax = gb$layout$panel_params[[1]]$x.range[2]
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax = gb$layout$panel_params[[1]]$y.range[2]
    c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
