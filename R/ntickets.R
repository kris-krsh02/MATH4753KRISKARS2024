#' Calculate Optimal Number of Tickets to Sell for Overbooking
#'
#' This function calculates the optimal number of tickets to sell for a flight,
#' using both the binomial distribution (discrete case) and the normal approximation (continuous case).
#'
#' @param N Integer. The number of seats on the flight.
#' @param gamma Numeric. The probability a plane will be truly overbooked.
#' @param p Numeric. The probability that any given ticket holder shows up.
#'
#' @return A list containing:
#' \describe{
#'   \item{nd}{The number of tickets to sell based on the binomial distribution (discrete case).}
#'   \item{nc}{The number of tickets to sell based on the normal approximation (continuous case).}
#'   \item{N}{The number of seats on the flight.}
#'   \item{gamma}{The probability of overbooking.}
#'   \item{p}{The probability that any given ticket holder shows up.}
#' }
#'
#' @details
#' The function calculates the number of tickets using two methods:
#' \itemize{
#'   \item \strong{Binomial Distribution}: Uses the exact binomial distribution to calculate the number of tickets.
#'   \item \strong{Normal Approximation}: Uses the normal approximation to the binomial distribution to calculate the number of tickets.
#' }
#'
#' @examples
#' # Example usage
#' ntickets(400, 0.02, 0.95)
#'
#'
#' @importFrom rootSolve uniroot.all
#' @importFrom stats pbinom pnorm
#' @importFrom graphics plot abline points
#' @export
ntickets <- function(N, gamma, p) {
  # Define upper limit as N + 10% of N
  upper_limit <- N + N/10

  # Discrete distribution objective function
  fn_objective_discrete <- function(n) {
    return(1 - gamma - pbinom(N, round(n), p))
  }

  # Find root for discrete distribution
  nd <- uniroot.all(fn_objective_discrete, lower = N, upper = upper_limit)
  nd <- ceiling(nd)  # Round up to nearest integer

  # Plot for the discrete distribution
  title_discrete <- paste("Objective vs n to find optimal tickets sold \n(", nd, ") for N=", N, ", gamma=", gamma, " discrete")
  plot(N:upper_limit, sapply(N:upper_limit, fn_objective_discrete), type = "l",
       xlab = "n", ylab = "Objective function", main = title_discrete)
  abline(v = nd, h = 0, col = "red")
  points(nd, 0, col = "red", pch = 19)
  points(N:upper_limit, sapply(N:upper_limit, fn_objective_discrete), pch = 21)

  # Normal approximation objective function
  fn_objective_normal <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    return(1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma))  # Normal approximation continuity correction
  }

  # Find root for normal approximation
  nc <- uniroot.all(fn_objective_normal, lower = N, upper = upper_limit)
  nc <- ceiling(nc)  # Round up to nearest integer

  # Plot for the normal approximation
  title_normal <- paste("Objective vs n to find optimal tickets sold \n(", nc, ") for N=", N, ", gamma=", gamma, " continuous")
  plot(N:upper_limit, sapply(N:upper_limit, fn_objective_normal), type = "l",
       xlab = "n", ylab = "Objective function", main = title_normal)
  abline(v = nc, h = 0, col = "red")
  points(nc, 0, col = "red", pch = 19)

  # Return results
  result <- list(nd = nd, nc = nc, N = N, gamma = gamma, p = p)
  return(result)
}
