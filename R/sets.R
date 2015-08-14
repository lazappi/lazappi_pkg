#' List Intersect
#'
#' Find the intersect of a list of vectors
#' 
#' @param set.list List of vectors to intersect
#' @param sort     Whether to sort the final set, default is TRUE
#' 
#' @return Vector containing the intersection
#' 
#' @examples
#' l <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'           Set3 = sample(20, 10), Set4 = sample(20, 10))
#' listIntersect(l)
#' 
#' @export
listIntersect <- function(set.list, sort = TRUE) {
    
    if (length(set.list) == 0) {
        inter.set <- c()
    } else if (length(set.list) == 1) {
        inter.set <- unique(unlist(set.list))
    } else if (length(set.list) == 2) {
        inter.set <- intersect(set.list[[1]], set.list[[2]])
    } else {
        inter.set <- intersect(set.list[[1]], listIntersect(set.list[-1]))
    }
    
    if (sort && length(inter.set > 0)) {inter.set <- sort(inter.set)}
    
    return(inter.set)
}

#' List Union
#' 
#' Find the union of a list of vectors
#' 
#' @param set.list List of vectors to union
#' @param sort     Whether to sort the final set, default is TRUE
#' 
#' @return Vector containing the union
#' 
#' @examples
#' l <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'           Set3 = sample(20, 10), Set4 = sample(20, 10))
#' listUnion(l)
#' 
#' @export
listUnion <- function(set.list, sort = TRUE) {
    
    if (length(set.list) == 0) {
        union.set <- c()
    } else if (length(set.list) == 1) {
        union.set <- unique(unlist(set.list))
    } else if (length(set.list) == 2) {
        union.set <- union(set.list[[1]], set.list[[2]])
    } else if (length(set.list) > 2) {
        union.set <- union(set.list[[1]], listUnion(set.list[-1]))
    }
    
    if (sort && length(union.set > 0)) {union.set <- sort(union.set)}
    
    return(union.set)
}

#' List Setdiff
#' 
#' Find the items that are in one list of vector but not another
#' 
#' @param set.list1 The first list of vectors to compare
#' @param set.list2 The second list of vectors to compare
#' @param sort      Whether to sort the final set, default is TRUE
#' 
#' @return List containing the items in set.list1 that are not in set.list2
#' 
#' @examples
#' l1 <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'            Set3 = sample(20, 10), Set4 = sample(20, 10))
#' l2 <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'            Set3 = sample(20, 10), Set4 = sample(20, 10))
#' listSetdiff(l1, l2)
#' 
#' @export
listSetdiff <- function(set.list1, set.list2, sort = TRUE) {
    
    set.list1.inter <- listIntersect(set.list1)
    set.list2.union <- listUnion(set.list2)
    
    diff.set <- setdiff(set.list1.inter, set.list2.union)
    
    if (sort && length(diff.set > 0)) {diff.set <- sort(diff.set)}
    
    return(diff.set)
}

#' Overlap Sets
#' 
#' Takes a list of vectors and returns the sets corresponding to the overlapping
#' regions in a Venn diagram
#' 
#' @param set.list List of vectors to overlap
#' @param sort     Whether to sort the final sets, default is TRUE
#' 
#' @return List of sets corresponding to Venn regions
#' 
#' @examples
#' l <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'           Set3 = sample(20, 10), Set4 = sample(20, 10))
#' overlapSets(l)
#' 
#' @export
overlapSets <- function(set.list, sort = TRUE) {
    
    # Get names, set if needed
    if (is.null(names(set.list))) {
        set.names <- seq(1, length(set.list))
    } else {
        set.names <- names(set.list)
    }
    
    # Get all possible combinations
    combos <- lapply(1:length(set.list),
                     function(j) combn(names(set.list), j, simplify = FALSE))
    combos <- unlist(combos, recursive = FALSE)
    
    # Set combination names
    names(combos) <- sapply(combos, function(i) paste0(i, collapse = "."))
    
    # Compute overlaps
    venn.sets <- lapply(combos,
                        function(combo) {
                            listSetdiff(set.list[combo],
                                        set.list[setdiff(set.names, combo)],
                                        sort = sort)
                        }) 
    
    return(venn.sets)
}

#' Combine Sets
#' 
#' Compute combination of a list of vectors using a selected set method
#' 
#' @param set.list List of vectors to combine
#' @param method   Method to combine sets, can be "union" or "intersect"
#'                 abbreviations accepted)
#' @param sort     Whether to sort the final sets, default is TRUE
#' 
#' @return List containing combined sets
#' 
#' @examples
#' l <- list(Set1 = sample(20, 10), Set2 = sample(20, 10),
#'           Set3 = sample(20, 10), Set4 = sample(20, 10))
#' combineSets(l, "union")
#' combineSets(l, "intersect")
#' 
#' @export
combineSets <- function(set.list, method = c("union", "intersect"),
                        sort = TRUE) {
    
    method <- match.arg(method)
    
    # Get names, set if needed
    if (is.null(names(set.list))) {
        set.names <- seq(1, length(set.list))
    } else {
        set.names <- names(set.list)
    }
    
    combos <- lapply(1:length(set.list),
                     function(j) combn(names(set.list), j, simplify = FALSE))
    combos <- unlist(combos, recursive = FALSE)
    
    # Set combination names
    names(combos) <- sapply(combos,
                            function(combo) {
                                paste0(method, ".",
                                       paste0(combo, collapse = "."))
                            })
    
    # Combine sets using method
    switch(method,
           union = {
               combo.sets <- lapply(combos,
                                    function(combo) {
                                        listUnion(set.list[combo], sort = sort)
                                    })
           },
           intersect = {
               combo.sets <- lapply(combos,
                                    function(combo) {
                                        listIntersect(set.list[combo],
                                                      sort = sort)
                                    })
           })
    
    return(combo.sets)
}