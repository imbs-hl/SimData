#' Simulate outcome from regression model
#'
#' Generates a binary or continuous outcome based on a logistic or linear
#' regression model, respectively. Each block (defined in \code{block_info}) can
#' have its own effect size. For each block the first principal component is
#' estimated and used as independent variable in the regression model.
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
#' @param type Character describing type of outcome ("binary" or "continuous")
#' @param sd Numeric stating standard deviation to use for normally distributed
#' error (default: 1)
#'
#' @return A vector of simulated outcomes (0/1 for binary).
#' @importFrom stats prcomp rbinom rnorm
#' @importFrom checkmate assert_choice
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(1000), ncol = 10)
#' block_info <- data.frame(
#'   block_id = c("A", "B"),
#'   block_size = c(5, 5),
#'   effect_size = c(1, 0.5)
#' )
#'
#' # binary outcome
#' y <- simulate_outcome(X, block_info, type = "binary")
#' table(y)
#'
#' # continuous outcome
#' y <- simulate_outcome(X, block_info, type = "continuous")
#' summary(y)
#' @export
simulate_outcome <- function(X, block_info, type = "binary", sd = 1) {

  assert_choice(
    type,
    c("binary", "continuous")
  )

  if (!all(c("block_id", "block_size", "effect_size") %in% colnames(block_info))) {
    stop("block_info must contain columns: block_id, block_size, and effect_size.")
  }

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

  if (type == "binary") {
    prob <- 1 / (1 + exp(-linear_predictor))
    outcome = rbinom(n, size = 1, prob = prob)
  } else if (type == "continuous") {
    outcome = as.numeric(linear_predictor) + rnorm(n, mean = 0, sd = sd)
  }

  return(outcome)
}
