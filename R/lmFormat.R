#' Format beta coefficient of lm() model object for LaTeX text
#'
#' @param x lm() model object
#' @param digits number of digits to round F statistic
#' @param ... arguments passed to \code{pFormat()}
#'
#' @return character string
#' 
#' @examples
#' mod <- lm(rnorm(50) ~ rnorm(50))
#' lmFormat(mod)
#' @return 
#' 
lmFormat <- function(x, digits = 2, ...){
  paste0("F(", 
    summary(x)$fstatistic[2],
    ",",
    summary(x)$fstatistic[3],
    ") = ",
    numFormat(summary(x)$fstatistic[1], digits = digits),
    ", ",
    pFormat(anova(x)$`Pr(>F)`[1], ...)
  )
}

