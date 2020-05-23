#' Scale Min-Max
#'
#' Scale a vector to be between 0 and 1
#'
#' @param x Continuous vector to scale
#' @param from Input range (vector of length two). If not given is calculated
#'        from the range for `x`
#'
#' @return Scaled vector
#'
#' @seealso scales::rescale
#'
#' @examples
#' x <- 0:10
#' scale_minmax(x)
#' scale_minmax(x, from = c(-10, 10))
#'
#' @export
scale_minmax <- function(x, from = range(x, na.rm = TRUE, finite = TRUE)) {
    scales::rescale(x, to = c(0, 1), from = from)
}

#' Scale Mean
#'
#' Mean scale a vector
#'
#' @param x Numeric vector to scale
#'
#' @return Scaled vector
#'
#' @references https://en.wikipedia.org/wiki/Feature_scaling#Mean_normalization
#'
#' @examples
#' scale_mean(1:10)
#' scale_mean(-5:10)
#'
#' @export
scale_mean <- function(x) {
    (x - mean(x)) / (max(x) - min(x))
}

#' Scale Unit Length
#'
#' Scale a vector to unit length
#'
#' @param x Numeric vector to scale
#'
#' @return Scaled vector
#'
#' @references https://en.wikipedia.org/wiki/Feature_scaling#Scaling_to_unit_length
#'
#' @examples
#' scale_unit(1:10)
#' scale_unit(-5:10)
#'
#' @export
scale_unit <- function(x) {
    x / (sqrt(sum(x ^ 2)))
}
