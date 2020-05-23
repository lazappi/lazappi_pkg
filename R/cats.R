#' A Cat Function
#'
#' This function allows you to express your love of cats. It comes from
#' Hilary Parker's ["Writing an R package from scratch"](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/).
#'
#' @param love Do you love cats? Defaults to TRUE.
#'
#' @export
#'
#' @examples
#' cat_function()
cat_function <- function(love = TRUE) {
    if (love == TRUE) {
        print("I love cats!")
    }
    else {
        print("I am not a cool person.")
    }
}
