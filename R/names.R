#' Name sequentially
#'
#' Sequentially name the items in an object. If names already exist they will be
#' kept but empty or `NA` names will be replaced.
#'
#' @param x Object to name
#' @param prefix Prefix string added to each new name
#'
#' @return Object with names
#' @export
#'
#' @examples
#' name_seq(1:3)
#' name_seq(c(A = 1, 2, 3), prefix = "Item")
name_seq <- function(x, prefix = "") {

    new_names <- names(x)
    seq_names <- paste0(prefix, seq_along(x))

    if (is.null(new_names)) {
        new_names <- seq_names
    }

    new_names[is.na(new_names)] <- seq_names[is.na(new_names)]
    new_names[new_names == ""] <- seq_names[new_names == ""]

    names(x) <- new_names
    return(x)
}
