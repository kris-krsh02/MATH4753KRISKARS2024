% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Calculate Optimal Number of Tickets to Sell for Overbooking}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{Integer. The number of seats on the flight.}

\item{gamma}{Numeric. The probability a plane will be truly overbooked.}

\item{p}{Numeric. The probability that any given ticket holder shows up.}
}
\value{
A list containing:
\describe{
  \item{nd}{The number of tickets to sell based on the binomial distribution (discrete case).}
  \item{nc}{The number of tickets to sell based on the normal approximation (continuous case).}
  \item{N}{The number of seats on the flight.}
  \item{gamma}{The probability of overbooking.}
  \item{p}{The probability that any given ticket holder shows up.}
}
}
\description{
This function calculates the optimal number of tickets to sell for a flight,
using both the binomial distribution (discrete case) and the normal approximation (continuous case).
}
\details{
The function calculates the number of tickets using two methods:
\itemize{
  \item \strong{Binomial Distribution}: Uses the exact binomial distribution to calculate the number of tickets.
  \item \strong{Normal Approximation}: Uses the normal approximation to the binomial distribution to calculate the number of tickets.
}
}
\examples{
# Example usage
ntickets(400, 0.02, 0.95)


}
