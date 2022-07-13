#' A sensible theme for basic ggplot output for use in LaTeX articles
#'
#' @param base_size base font size, given in pts
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param ... further arguments passed to \code{ggplot2::theme()}
#' 
#' @examples
#' data(mtcars)
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt)) + 
#'  geom_point() + 
#'  themePub()
#' @import ggplot2
#' @export
#' 
themePub <- function(base_size = 12, base_family = "serif", 
  base_line_size = base_size/22, base_rect_size = base_line_size, ...) { 
  ggplot2::theme_minimal(base_size = base_size, 
    base_family = base_family,
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size) %+replace%
  ggplot2::theme(
    panel.border = element_rect(size = base_line_size, fill = NA),
    legend.box.background = element_rect(size = base_line_size, fill = NA),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    strip.background = element_rect(),
  ) %+replace%
  ggplot2::theme(...)
}
