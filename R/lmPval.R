#' Get the model P-value for a linear model
#'
#' @param x lm model object
#'
#' @return p value of model
#' 
#' @export
#' 
lmPval <- function (x) {
    if (class(x) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(x)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
