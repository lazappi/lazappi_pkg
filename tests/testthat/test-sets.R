test_that("intersect_list works", {
    expect_identical(
        intersect_list(list(c(1, 2, 3), c(2, 3, 4), c(3, 4, 5))),
        3
    )
})

test_that("union_list works", {
    expect_identical(
        union_list(list(c(1, 2, 3), c(2, 3, 4), c(3, 4, 5))),
        c(1, 2, 3, 4, 5)
    )
})

test_that("setdiff_lists works", {
    expect_identical(
        setdiff_lists(list(c(1, 2, 3), c(2, 3, 4)), list(c(1, 2, 4))),
        3
    )
})

test_that("venn_sets works", {
    expect_identical(
        venn_sets(list(c(1, 2, 3), c(2, 3, 4))),
        list(Set1 = 1, Set2 = 4, Set1_Set2 = c(2, 3))
    )
})

test_that("combine_sets works", {
    expect_identical(
        combine_sets(list(c(1, 2, 3), c(2, 3, 4)), method = "union"),
        list(Set1 = c(1, 2, 3), Set2 = c(2, 3, 4), Set1_Set2 = c(1, 2, 3, 4))
    )
    expect_identical(
        combine_sets(list(c(1, 2, 3), c(2, 3, 4)), method = "intersect"),
        list(Set1 = c(1, 2, 3), Set2 = c(2, 3, 4), Set1_Set2 = c(2, 3))
    )
})

test_that("all_combos works", {
    expect_identical(
        all_combos(c(1, 2, 3)),
        list(
            Length1 = list(
                Combo1 = 1,
                Combo2 = 2,
                Combo3 = 3),
            Length2 = list(
                Combo1 = c(1, 2),
                Combo2 = c(1, 3),
                Combo3 = c(2, 3)),
            Length3 = list(
                Combo1 = c(1, 2, 3)
            )
        )
    )
})
