% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mybin.R
\name{mybin}
\alias{mybin}
\title{Simulate and Visualize a Binomial Distribution}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
\arguments{
\item{iter}{number of iterations of the experiment}

\item{n}{size of the sample}

\item{p}{probability of success in each trial}
}
\value{
A numeric vector with the proportions of each possible number of successes (from 0 to `n`)
over the `iter` iterations.
}
\description{
Simulate and Visualize a Binomial Distribution
}
\examples{
# Simulate a binomial distribution with 500 iterations, 20 trials per iteration,
# and 70\% success rate
mybin(iter = 500, n = 20, p = 0.7)
}
