source("../lab.R", chdir = TRUE)
library(testthat)

warning('Problem 1 not checked automatically')

test_that("2.1. How many observations?", {
    expect_equal(problem2.1, 2930L)
})

test_that("2.2. How many variables?", {
    expect_equal(problem2.2.factors, 0L)
    expect_equal(problem2.2.characters, 46L)
    expect_equal(problem2.2.numerics, 35L)
})

test_that("2.4. Missing values", {
    expect_equal(problem2.4, 0L)
})

warning('Problem 3 not checked automatically')

test_that('4. Duplicate rows', {
    expect_equal(nrow(dataf_nodup), 2930L)
})

test_that('5.2. Count of houses by overall condition', {
    expect_equal(nrow(cond_count), 9L)
})

correct_levels = c('Very_Poor', 'Poor', 
                   'Fair', 'Below_Average', 
                   'Average', 'Above_Average', 
                   'Good', 'Very_Good', 
                   'Excellent', 'Very_Excellent')

test_that('5.3. Condition levels', {
    expect_identical(condition_levels, correct_levels)
})

test_that('5.4. Overall condition factor with the correct levels', {
    expect_identical(good_factor, factor(dataf$Overall_Cond, 
                                         levels = correct_levels))
})

dataf_fct = mutate(dataf, overall_cond_fct = factor(Overall_Cond, 
                                                    levels = correct_levels))

test_that('5.5. `overall_cond_fct` is a factor in `dataf` with the correct levels', {
    expect_identical(dataf$overall_cond_fct, dataf_fct$overall_cond_fct)
})

test_that('6.1. `dataf_smol` contains all the factor and numeric variables from `dataf`', {
    expect_identical(dataf_smol, select(dataf_fct, where(is.factor), where(is.numeric)))
})


test_that('6.4. Top 10 variables most strongly correlated with sale price', {
    top_10_soln = structure(list(covar = c("Sale_Price", "Gr_Liv_Area", "Garage_Cars", 
                                           "Year_Built", "Garage_Area", "Full_Bath",
                                           "Total_Bsmt_SF", "Year_Remod_Add", 
                                           "First_Flr_SF", "Fireplaces"), 
                                 Sale_Price = c(1, 0.723342036109249, 
                                                0.701545429698005, 0.680822358625856, 
                                                0.660474678142175, 0.634160817015663, 
                                                0.606564291637224, 0.601453915476304, 
                                                0.581535848633275, 0.526137194835603
                                 )), 
                            row.names = c(NA, -10L), 
                            class = c("tbl_df", "tbl", "data.frame"
                            ))
    
    expect_equal(top_10, top_10_soln)
})





