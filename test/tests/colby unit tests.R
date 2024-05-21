library(testthat)


# tests:
test_that("chooseType logic works", {
  
  expect_true(chooseType(c(NA,0:10)) == "nonnegative")
  expect_true(chooseType(c(NA, 3:10)) == "nonnegative")
  expect_true(chooseType(c(NA, 4:10)) == "arbitrary")
  expect_true(chooseType(c(NA, -3:5)) == "centered")
  expect_true(chooseType(c(NA, -3:7)) == "arbitrary")
})


# tests:
test_that("colby runs without errors under diverse conditions", {
  
  # centered:
  res <- colby(rnorm(100), type = NULL, cols = NULL, quant = 0.99, log = FALSE, na_col = NA) 
  expect_true(sum(!is.na(res$col)) == 100)
  
  # non-negative:
  res <- colby(0:99, type = NULL, cols = NULL, quant = 0.99, log = FALSE, na_col = NA) 
  expect_true(sum(!is.na(res$col)) == 100)
  
  # arbitrary:
  res <- colby(-10:89, type = NULL, cols = NULL, quant = 0.99, log = FALSE, na_col = NA) 
  expect_true(sum(!is.na(res$col)) == 100)
})
  