#' Full Simulation Framework with Flexible Block Structure
#'
#' Simulates correlated predictors and a binary outcome using flexible
#' block definitions, correlations, and block-specific effects.
#' The returned object includes full metadata on block structure and effects.
#'
#' @param n Integer. Number of observations.
#' @param block_info Data frame describing all blocks. Must include columns:
#'   \code{block_id}, \code{block_size}, \code{rho}, and \code{effect_size}.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{data}: Data frame with the simulated outcome and predictors.
#'     \item \code{block_info}: Data frame with block structure and effect 
#'     information.
#'   }
#' @examples
#' set.seed(42)
#' block_info <- data.frame(
#'   block_id = c("A", "B", "C"),
#'   block_size = c(4, 6, 3),
#'   rho = c(0.4, 0.1, 0.5),
#'   effect_size = c(1, 0, 0.7)
#' )
#' result <- simulate_data_flexible(n = 300, block_info = block_info)
#' head(result$data)
#' result$block_info
#' @export
simulate_data_flexible <- function(n = 1000, block_info) {
  
  blocks <- define_blocks(block_info)
  X <- simulate_predictors(blocks, n)
  y <- simulate_outcome_pca(X, block_info)
  
  block_indices <- split(
    1:ncol(X), 
    rep(block_info$block_id, 
        times = block_info$block_size))
  var_metadata <- data.frame(
    var_name = colnames(X),
    block_id = rep(block_info$block_id, times = block_info$block_size),
    effect_size = rep(block_info$effect_size, times = block_info$block_size)
  )
  
  return(
    list(
      data = data.frame(y = y, X),
      block_info = block_info,
      var_metadata = var_metadata
    )
  )
}