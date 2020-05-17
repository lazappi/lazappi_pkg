#' Intersect list
#'
#' Find the intersection of a list of vectors
#'
#' @param x List of vectors to intersect
#' @param sort Whether to sort the intersection
#'
#' @return Vector containing the intersection of all vectors in `x`
#' @export
#'
#' @examples
#' x <- list(A = c(1, 2, 3), B = c(2, 3, 4), C = c(3, 4, 5))
#' intersect_list(x)
intersect_list <- function(x, sort = TRUE) {

    intersection <- purrr::reduce(x, intersect)

    if (sort) {
        intersection <- sort(intersection)
    }

    return(intersection)
}


#' Union list
#'
#' Find the union of a list of vectors
#'
#' @param x List of vectors to union
#' @param sort Whether to sort the union
#'
#' @return Vector containing the union of all vectors in `x`
#' @export
#'
#' @examples
#' x <- list(A = c(1, 2, 3), B = c(2, 3, 4), C = c(3, 4, 5))
#' union_list(x)
union_list <- function(x, sort = TRUE) {

    unioned <- purrr::reduce(x, union, .init = c())

    if (sort) {
        unioned <- sort(unioned)
    }

    return(unioned)
}

#' Set difference lists
#'
#' Find the set of items that are present in the intersection of one list of
#' vectors but not present in the union of another list of vectors
#'
#' @param x List of vectors
#' @param y Another list of vectors
#' @param sort Whether to sort the difference
#'
#' @return Vector containing difference between two lists of vectors
#' @export
#'
#' @examples
#' x <- list(A = c(1, 2, 3), B = c(2, 3, 4), C = c(3, 4, 5))
#' y <- list(D = c(1, 2), E = c(4, 5))
#' setdiff_lists(x, y)
setdiff_lists <- function(x, y, sort = TRUE) {

    intersection_x <- intersect_list(x, sort = sort)
    union_y <- union_list(y, sort = sort)

    diff_x <- setdiff(intersection_x, union_y)

    return(diff_x)
}

#' Venn sets
#'
#' Takes a list of vectors and returns the sets corresponding to the overlapping
#' regions in a Venn diagram
#'
#' @param sets List of vectors
#' @param sort Whether to sort the overlaps
#'
#' @return Named list of overlap vectors
#' @export
#'
#' @examples
#' x <- list(A = c(1, 2, 3), B = c(2, 3, 4), C = c(3, 4, 5))
#' venn_sets(x)
venn_sets <- function(sets, sort = TRUE) {

    sets <- name_seq(sets, "Set")

    combos <- unlist(all_combos(names(sets)), recursive = FALSE)
    names(combos) <- purrr::map_chr(combos, paste0, collapse = "_")

    venn <- purrr::map(combos, function(.combo) {
        setdiff_lists(
            sets[.combo], sets[setdiff(names(sets), .combo)],
            sort = sort
        )
    })

    return(venn)
}

#' Combine sets
#'
#' Compute all combinations of a list of vectors using a selected set method
#'
#' @param sets List of vectors
#' @param method Method for combining sets
#' @param sort Whether to sort the results
#'
#' @return Named list of combined vectors
#' @export
#'
#' @examples
#' x <- list(A = c(1, 2, 3), B = c(2, 3, 4), C = c(3, 4, 5))
#' combine_sets(x, method = "union")
#' combine_sets(x, method = "intersect")
combine_sets <- function(sets, method = c("union", "intersect"), sort = TRUE) {

    method <- match.arg(method)

    sets <- name_seq(sets, "Set")

    combos <- unlist(all_combos(names(sets)), recursive = FALSE)
    names(combos) <- purrr::map_chr(combos, paste0, collapse = "_")

    comb_fun <- switch(
        method,
        union     = union_list,
        intersect = intersect_list
    )

    combinations <- purrr::map(combos, ~ comb_fun(sets[.x]), sort = sort)

    return(combinations)
}


#' All combinations
#'
#' Compute all possible combinations of items in a vector
#'
#' @param x Vector to get combinations of
#' @param min Minimum number of items in a combination
#' @param max Maximum number of items in a combination
#'
#' @return Nested named list of combinations. The first level of the list is the
#' combination lengths and the second level is the combinations themselves
#' @export
#'
#' @importFrom utils combn
#'
#' @examples
#' all_combos(1:3)
#'
#' # Single level list of combinations
#' unlist(all_combos(1:3), recursive = FALSE)
all_combos <- function(x, min = 1, max = length(x)) {

    lengths <- seq(min, max)

    combos <- purrr::map(lengths, function(.len) {
        name_seq(combn(x, .len, simplify = FALSE), "Combo")
    })

    combos <- name_seq(combos, "Length")

    return(combos)
}
