#' Simulate Binary Outcome from Logistic Regression
#'
#' Generates a binary outcome based on a logistic regression model,
#' where each block (defined in \code{block_info}) can have its own effect size.
#'
#' @param X Numeric matrix of predictors (output from
#' \code{simulate_predictors()}).
#' @param block_info Data frame describing block structure. Must contain:
#'   \itemize{
#'     \item \code{block_id}: Unique identifier for each block.
#'     \item \code{block_size}: Number of variables per block.
#'     \item \code{effect_size}: Numeric effect size for the block (0 for
#'     noise blocks).
#'   }
#'
#' @return A binary vector of simulated outcomes (0/1).
#' @importFrom stats rbinom
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(1000), ncol = 10)
#' block_info <- data.frame(
#'   block_id = c("A", "B"),
#'   block_size = c(5, 5),
#'   effect_size = c(1, 0.5)
#' )
#' y <- simulate_outcome(X, block_info)
#' table(y)
#' @export
simulate_outcome <- function(X, block_info) {
  if (!all(c("block_id", "block_size", "effect_size") %in% colnames(block_info))) {
    stop("block_info must contain columns: block_id, block_size, and effect_size.")
  }

  # Derive block-to-column mapping
  block_indices <- split(
    1:ncol(X),
    rep(block_info$block_id, times = block_info$block_size))

  linear_predictor <- rep(0, nrow(X))
  for (i in seq_len(nrow(block_info))) {
    block_id <- block_info$block_id[i]
    beta <- block_info$effect_size[i]
    if (beta != 0) {
      cols <- block_indices[[block_id]]
      linear_predictor <- linear_predictor +
        as.vector(X[, cols, drop = FALSE] %*% rep(beta, length(cols)))
    }
  }

  prob <- 1 / (1 + exp(-linear_predictor))
  rbinom(n = nrow(X), size = 1, prob = prob)
}


#' @importFrom stats prcomp rbinom
#'
simulate_outcome_pca <- function(X, block_info) {
  block_indices <- split(1:ncol(X), rep(block_info$block_id, times = block_info$block_size))
  n <- nrow(X)
  pc_matrix <- matrix(NA, nrow = n, ncol = length(block_indices))
  colnames(pc_matrix) <- names(block_indices)

  for (i in seq_along(block_indices)) {
    block_vars <- X[, block_indices[[i]], drop = FALSE]
    # Apply PCA to the block variables, center and scale TRUE for stability
    pca <- prcomp(block_vars, center = TRUE, scale. = TRUE)
    # Take first principal component
    pc_matrix[, i] <- pca$x[, 1]
  }

  # Use block_info effect sizes to weight PCs (0 for noise blocks)
  effect_sizes <- if ("effect_size" %in% names(block_info)) {
    block_info$effect_size
  } else {
    rep(0, nrow(block_info))
  }
  linear_predictor <- pc_matrix %*% effect_sizes
  prob <- 1 / (1 + exp(-linear_predictor))
  rbinom(n, size = 1, prob = prob)
}
