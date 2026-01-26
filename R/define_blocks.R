#' Define Correlated Predictor Blocks
#'
#' Creates a list of covariance matrices representing blocks of correlated 
#' predictors.
#' Each block can have a different size and correlation level, specified via a 
#' data frame.
#'
#' @param block_info A data frame with one row per block. Must contain:
#'   \itemize{
#'     \item \code{block_id}: Unique numeric or character label for each block.
#'     \item \code{block_size}: Integer, number of variables in the block.
#'     \item \code{rho}: Numeric, pairwise correlation within the block.
#'   }
#'
#' @return A named list of covariance matrices, each corresponding to a block.
#' @examples
#' block_info <- data.frame(
#'   block_id = c("A", "B", "C"),
#'   block_size = c(4, 6, 3),
#'   rho = c(0.4, 0.2, 0.6)
#' )
#' blocks <- define_blocks(block_info)
#' str(blocks)
#' @export
define_blocks <- function(block_info) {
  if (!all(c("block_id", "block_size", "rho") %in% colnames(block_info))) {
    stop("block_info must contain columns: block_id, block_size, and rho.")
  }
  
  blocks <- vector("list", nrow(block_info))
  names(blocks) <- block_info$block_id
  
  for (i in 1:nrow(block_info)) {
    size <- block_info$block_size[i]
    rho <- block_info$rho[i]
    Sigma <- matrix(rho, nrow = size, ncol = size)
    diag(Sigma) <- 1
    blocks[[i]] <- Sigma
  }
  
  return(blocks)
}