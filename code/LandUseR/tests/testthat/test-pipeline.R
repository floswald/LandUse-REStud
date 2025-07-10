
context("final pipeline")

x = read_output()

test_that("test years", {
    expect_true(x[, all(unique(year) == c(1876,1950,GHS_years(), 2016))])
})

test_that("test nrows", {
    expect_equal(nrow(x), 100 * (length(c(1876,1950,GHS_years(), 2016))))
})

test_that("rank variable contiguous", {
    expect_true(all(x[, rank] == rep(1:100,7)))
})

test_that("paris area correct in first year", {
  expect_equal(x[CODGEO == "75060" & year == 1876, area], 57.54)
})
