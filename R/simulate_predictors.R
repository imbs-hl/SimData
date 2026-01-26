#' Simulate Predictors from Multivariate Normal Blocks
#'
#' Generates predictor variables for each block using a multivariate normal
#' distribution.
#'
#' @param blocks List of covariance matrices (output from \code{define_blocks()}).
#' @param n Integer. Number of observations to simulate per block.
#'
#' @return A numeric matrix of simulated predictors.
#' @examples
#' block_info <- data.frame(
#'   block_id = c("A", "B"),
#'   block_size = c(2, 3),
#'   rho = c(0.3, 0.2)
#' )
#' blocks <- define_blocks(block_info)
#' X <- simulate_predictors(blocks, n = 100)
#' head(X)
#' @importFrom MASS mvrnorm
#' @export
simulate_predictors <- function(blocks, n) {
  predictors <- list()
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    block_data <- MASS::mvrnorm(n = n, mu = rep(0, ncol(block)), Sigma = block)
    colnames(block_data) <- paste0("var_", i, "_", 1:ncol(block))
    predictors[[i]] <- block_data
  }
  data_matrix <- do.call(cbind, predictors)
  return(data_matrix)
}
