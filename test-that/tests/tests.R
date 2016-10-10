# load the source code of the functions to be tested
source("../functions/functions.R")

# context with one test that groups expectations
context("Test for range value") 

test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(range_value(x), 4)
  expect_length(range_value(x), 1)
  expect_type(range_value(x), 'double')
})

test_that("range works with missing values", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(range_value(y, na.rm=TRUE), 3)
  expect_length(range_value(y, na.rm=TRUE), 1)
  expect_type(range_value(y, na.rm=TRUE), 'double')
})

test_that("range works with true/false values", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(range_value(z), 1L)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), 'integer')
})

test_that("range gives an error with letters", {
  w <- letters[1:5]
  
  expect_error(range_value(w), 'non-numeric argument')
})

#####

# context with one test that groups expectations
context("Test for missing value") 

test_that("missing value works as expected", {
  m <- c(1, 2, 3)
  
  expect_gt(missing_values(m), 0)
  #expect_length(missing_value(m), 1)
  #expect_type(missing_value(m), 'double')
})

