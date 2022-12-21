#' Visualise all colours in qualPal 
#'
#' @return Plot object
#' 
palVisAll <- function() {
  pals <- qualPal()

  par(mfrow = c(length(qualPal()), 1), oma = rep(0, 4), mar = rep(0.2, 4))

  lapply(seq_along(pals), function(x) {
    pal <- pals[[x]]
    name <- names(pals)[x]
    palShow(pal)
    text(1,1, name, pos = 4)
  })
}

