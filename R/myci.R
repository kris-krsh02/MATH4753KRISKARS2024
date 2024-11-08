#' Compute a 95\% Confidence Interval for the Mean
#'
#' Calculates a 95\% confidence interval for the mean of a sample.
#'
#' @param d Numeric vector of sample data.
#' @return Numeric vector of length 2 with the lower and upper bounds of the confidence interval.
#'
#' @examples
#' sample_data <- c(5, 7, 8, 9, 10)
#' myci(sample_data)
#'
#' @importFrom stats qt
#' @export
myci <- function(d) {
  n <- length(d)
  mean_d <- mean(d)
  sd_d <- sd(d)
  t <- qt(1 - 0.05 / 2, n - 1)
  ci <- c(mean_d - t * sd_d / sqrt(n), mean_d + t * sd_d / sqrt(n))
  return(ci)
}
