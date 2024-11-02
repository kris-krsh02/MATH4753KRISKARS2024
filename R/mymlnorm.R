#' Maximum Likelihood Estimation for Parameters of a Normal Distribution
#'
#' `mymlnorm` estimates the parameters `mu` (mean) and `sigma` (standard deviation) of a normal distribution
#' by calculating the maximum likelihood based on a sample vector `x`. This function evaluates the
#' log-likelihood of each combination of `mu` and `sigma` values provided as input vectors, identifies
#' the maximum likelihood estimates, and visualizes the likelihood surface with a contour plot.
#'
#' The log-likelihood for a normal distribution is computed by summing the log of the normal density for each
#' data point in `x`, given parameters `mu` and `sigma`. The function applies this log-likelihood calculation
#' across all combinations of `mu` and `sigma`, stores the results in a matrix, and exponentiates it to return
#' to the original likelihood scale for contour plotting. The maximum likelihood estimates are marked on the plot
#' with a vertical line at `mu` and a horizontal line at `sigma`.
#'
#' @param x A numeric vector representing the sample data.
#' @param mu A numeric vector of possible values for the mean (`mu`) to evaluate the likelihood.
#' @param sig A numeric vector of possible values for the standard deviation (`sigma`) to evaluate the likelihood.
#' @param ... Additional graphical parameters to pass to the `contour` function.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{x}{The input sample vector `x`.}
#'   \item{coord}{A matrix indicating the indices of the maximum likelihood estimates for `mu` and `sigma`.}
#'   \item{maxl}{The maximum likelihood value (non-log-transformed).}
#' }
#'
#' @examples
#' # Sample data generation
#' x <- rnorm(100, mean = 5, sd = 2)
#'
#' # Define ranges for mu and sigma
#' mu_values <- seq(4, 6, length = 100)
#' sigma_values <- seq(1, 3, length = 100)
#'
#' # Maximum likelihood estimation with contour plot
#' result <- mymlnorm(x, mu = mu_values, sig = sigma_values)
#'
#' @importFrom stats dnorm sd
#' @importFrom graphics contour abline points
#'
#' @export
mymlnorm <- function(x, mu, sig, ...) {
  # x sample vector
  nmu <- length(mu) # number of values in mu
  nsig <- length(sig)
  n <- length(x) # sample size
  zz <- c()    ## initialize a new vector

  # Define log-likelihood function for normal distribution
  lfun <- function(x, m, p)
    log(dnorm(x, mean = m, sd = p))

  # Evaluate likelihoods for each sigma
  for (j in 1:nsig) {
    z <- outer(x, mu, lfun, p = sig[j]) # z is a matrix
    y <- apply(z, 2, sum) # Summing log-likelihood values for each mu
    zz <- cbind(zz, y)
  }

  maxl <- max(exp(zz)) # Maximum likelihood value
  coord <- which(exp(zz) == maxl, arr.ind = TRUE) # Coordinates of max likelihood
  maxlsig <- apply(zz, 1, max)

  # Contour plot of likelihood surface
  contour(
    mu,
    sig,
    exp(zz),
    las = 3,
    xlab = expression(mu),
    ylab = expression(sigma),
    axes = TRUE,
    main = expression(paste("L(", mu, ",", sigma, ")", sep = "")),
    ...
  )

  # Theoretical estimates for visual reference
  mlx <- round(mean(x), 2)
  mly <- round(sqrt((n - 1) / n) * sd(x), 2)
  abline(v = mlx, lwd = 2, col = "Green")
  abline(h = mly, lwd = 2, col = "Red")

  # Maximum likelihood estimates from grid search
  muest <- mu[coord[1]]
  sigest <- sig[coord[2]]
  abline(v = muest, h = sigest)

  return(list(x = x, coord = coord, maxl = maxl))
}
