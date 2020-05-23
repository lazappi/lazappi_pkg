test_that("name_seq works", {
    expect_identical(name_seq(c(1, 2, 3)), c("1" = 1, "2" = 2, "3" = 3))
    expect_identical(
        name_seq(c(1, 2, 3), prefix = "Item"),
        c("Item1" = 1, "Item2" = 2, "Item3" = 3)
    )
    expect_identical(
        name_seq(c(A = 1, 2, 3), prefix = "Item"),
        c("A" = 1, "Item2" = 2, "Item3" = 3)
    )
})
