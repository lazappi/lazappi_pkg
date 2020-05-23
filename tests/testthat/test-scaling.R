test_that("scale_minmax works", {
    expect_identical(scale_minmax(c(0, 10)), c(0, 1))
    expect_identical(scale_minmax(c(-1, 1)), c(0, 1))
})

test_that("scale_mean works", {
    expect_identical(scale_mean(c(0, 1)), c(-0.5, 0.5))
    expect_identical(scale_mean(c(-1, 1)), c(-0.5, 0.5))
})

test_that("scale_unit works", {
    expect_identical(scale_unit(c(0, 1)), c(0, 1))
    expect_identical(scale_unit(c(-1,  1)), c(- 1 / sqrt(2), 1 / sqrt(2)))
})
