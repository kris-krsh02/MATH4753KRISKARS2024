#' Central Limit Theorem Simulation
#'
#' This function simulates the Central Limit Theorem (CLT) by generating random samples
#' from a uniform distribution between 0 and 5, and calculating the means of these samples
#' over multiple iterations.
#'
#' @param n Integer. The number of observations per iteration (sample size).
#' @param iter Integer. The number of iterations (number of samples).
#'
#' @return A vector containing the means of the samples generated in each iteration.
#' It also generates a histogram of the sample means.
#'
#' @details The function generates \code{n} observations \code{iter} times from a uniform
#' distribution ranging from 0 to 5. For each sample, it computes the mean and then
#' plots a histogram of these means. This demonstrates the Central Limit Theorem (CLT),
#' as the distribution of sample means tends to be normal as the number of iterations
#' increases, regardless of the distribution of the original data.
#'
#' @importFrom stats runif
#' @importFrom graphics hist
#'
#' @examples
#' # Simulate CLT with sample size 10 and 10000 iterations
#' myclt(n = 10, iter = 10000)
#'
#' @export
myclt <- function(n, iter) {
  y <- runif(n * iter, 0, 5)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  y_bar <- apply(data, 2, mean)
  hist(y_bar)
  return(y_bar)
}
