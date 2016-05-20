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

#' Scale Range
#'
#' Scale a vector to be between a new range
#'
#' @param x Numeric vector to scale
#' @param newMin New minimum value
#' @param newMax New maximum value
#' @param ... Extra parameter passed to min and max (eg na.rm = TRUE)
#'
#' @return Scaled vector
#'
#' @references https://stackoverflow.com/questions/5468280/scale-a-series-between-two-points
#'
#' @seealso scales::rescale
#'
#' @examples
#' x <- 0:10
#' scaleRange(x)
#' scaleRange(x, -2, 2)
#' scaleRange(c(x, NA))
#' scaleRange(c(x, NA), na.rm = TRUE)
#'
#' @export
scaleRange <- function(x, newMin = 0, newMax = 1, ...) {
    (x - min(x, ...)) / (max(x, ...) - min(x, ...)) * (newMax - newMin) + newMin
}