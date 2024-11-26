% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymlnorm.R
\name{mymlnorm}
\alias{mymlnorm}
\title{Maximum Likelihood Estimation for Parameters of a Normal Distribution}
\usage{
mymlnorm(x, mu, sig, ...)
}
\arguments{
\item{x}{A numeric vector representing the sample data.}

\item{mu}{A numeric vector of possible values for the mean (`mu`) to evaluate the likelihood.}

\item{sig}{A numeric vector of possible values for the standard deviation (`sigma`) to evaluate the likelihood.}

\item{...}{Additional graphical parameters to pass to the `contour` function.}
}
\value{
A list with the following components:
\describe{
  \item{x}{The input sample vector `x`.}
  \item{coord}{A matrix indicating the indices of the maximum likelihood estimates for `mu` and `sigma`.}
  \item{maxl}{The maximum likelihood value (non-log-transformed).}
}
}
\description{
`mymlnorm` estimates the parameters `mu` (mean) and `sigma` (standard deviation) of a normal distribution
by calculating the maximum likelihood based on a sample vector `x`. This function evaluates the
log-likelihood of each combination of `mu` and `sigma` values provided as input vectors, identifies
the maximum likelihood estimates, and visualizes the likelihood surface with a contour plot.
}
\details{
The log-likelihood for a normal distribution is computed by summing the log of the normal density for each
data point in `x`, given parameters `mu` and `sigma`. The function applies this log-likelihood calculation
across all combinations of `mu` and `sigma`, stores the results in a matrix, and exponentiates it to return
to the original likelihood scale for contour plotting. The maximum likelihood estimates are marked on the plot
with a vertical line at `mu` and a horizontal line at `sigma`.
}
\examples{
# Sample data generation
x <- rnorm(100, mean = 5, sd = 2)

# Define ranges for mu and sigma
mu_values <- seq(4, 6, length = 100)
sigma_values <- seq(1, 3, length = 100)

# Maximum likelihood estimation with contour plot
result <- mymlnorm(x, mu = mu_values, sig = sigma_values)

}
