library(testthat)
library(SimData)

test_that("define_blocks creates correct covariance matrices", {
  block_info <- data.frame(block_id = c("A","B"),
                           block_size = c(2,3),
                           rho = c(0.5,0.2))
  blocks <- define_blocks(block_info)
  expect_equal(length(blocks), 2)
  expect_equal(dim(blocks$A), c(2,2))
  expect_equal(blocks$B[1,2], 0.2)
})

test_that("simulate_predictors returns matrix with correct dimensions", {
  blocks <- list(A = matrix(c(1,0.3,0.3,1), 2, 2))
  X <- simulate_predictors(blocks, n = 10)
  expect_equal(dim(X), c(10,2))
})

test_that("simulate_outcome returns binary outcomes", {
  X <- matrix(rnorm(20), 10, 2)
  block_info <- data.frame(block_id = "A", block_size = 2, effect_size = 1)
  y <- simulate_outcome(X, block_info)
  expect_true(all(y %in% c(0,1)))
})
