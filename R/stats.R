#' Finite Mean
#' 
#' Calculate mean of a vector excluding infinite and NA values
#' 
#' @param x Numeric vector to calculate mean
#' 
#' @return Mean value
#' 
#' @examples 
#' mean(c(1, 2, 4, 2, Inf))
#' finiteMean(c(1, 2, 4, 2, Inf))
#' mean(c(1, 2, 4, 2, Inf, NA))
#' finiteMean(c(1, 2, 4, 2, Inf, NA))
#' 
#' @export
finiteMean <- function(x) {
    mean(x[is.finite(x)])
}