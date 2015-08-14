#' List Intersect
#'
#' Find the intersect of a list of vectors
#' 
#' @param set.list List of vectors to intersect
#' 
#' @return Vector containing the intersection
#' 
#' @export
listIntersect <- function(set.list) {
    
    if (length(set.list) == 0) {
        inter.set <- c()
    } else if (length(set.list) == 1) {
        inter.set <- unique(unlist(set.list))
    } else if (length(set.list) == 2) {
        inter.set <- intersect(set.list[[1]], set.list[[2]])
    } else {
        inter.set <- intersect(set.list[[1]], listIntersect(set.list[-1]))
    }
    
    return(inter.set)
}

#' List Union
#' 
#' Find the union of a list of vectors
#' 
#' @param set.list List of vectors to union
#' 
#' @return Vector contining the union
#' 
#' @export
listUnion <- function(set.list) {
    
    if (length(set.list) == 0) {
        union.set <- c()
    } else if (length(set.list) == 1) {
        union.set <- unique(unlist(set.list))
    } else if (length(set.list) == 2) {
        union.set <- union(set.list[[1]], set.list[[2]])
    } else if (length(set.list) > 2) {
        union.set <- union(set.list[[1]], listUnion(set.list[-1]))
    }
    
    return(union.set)
}

#' List Setdiff
#' 
#' Find the items that are in one list of vector but not another
#' 
#' @param set.list1 The first list of vectors to compare
#' @param set.list2 The second list of vectors to compare
#' 
#' @return
#' 
#' @export
listSetdiff <- function(set.list1, set.list2) {
    
    set.list1.inter <- listIntersect(set.list1)
    set.list2.union <- listUnion(set.list2)
    
    diff.set <- setdiff(set.list1.inter, set.list2.union)
    
    return(diff.set)
}

#' Overlap Sets
#' 
#' Takes a list of vectors and returns the sets corresponding to the overlapping
#' regions in a Venn diagram
#' 
#' @param set.list List of vectors to overlap
#' 
#' @return List of sets corresponding to Venn regions
overlapSets <- function(set.list) {
    
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
                                        set.list[setdiff(set.names, combo)])
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
#' 
#' @return List containing combined sets
#' 
#' @export
combineSets <- function(set.list, method = c("union", "intersect")) {
    
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
                                        listUnion(set.list[combo])
                                    })
           },
           intersect = {
               combo.sets <- lapply(combos,
                                    function(combo) {
                                        listIntersect(set.list[combo])
                                    })
           })
    
    return(combo.sets)
}