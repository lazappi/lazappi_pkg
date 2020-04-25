test_that("loving cats works", {
    expect_equal(invisible(cat_function()), "I love cats!")
})

test_that("not loving cats works", {
    expect_equal(cat_function(love = FALSE), "I am not a cool person.")
})
