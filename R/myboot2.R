#' Bootstrap Confidence Interval Calculation with Histogram Plot
#'
#' @description
#' This function performs a bootstrap sampling procedure to estimate the confidence interval of a specified statistic (e.g., mean, median) from a given sample. It also generates a histogram of the bootstrap sample statistics.
#'
#' @param iter Integer. Number of bootstrap iterations. Default is 10,000.
#' @param x Numeric vector. The sample data from which to bootstrap.
#' @param fun Character. The statistic to be calculated in each bootstrap sample. Default is `"mean"`.
#' @param alpha Numeric. Significance level for the confidence interval. Default is 0.05.
#' @param cx Numeric. The size of the text annotations in the plot. Default is 1.5.
#' @param ... Additional graphical parameters to be passed to `hist()`.
#'
#' @details
#' The function performs `iter` bootstrap resamples of the input sample `x`, applying the function specified in `fun` to each sample. The output includes the confidence interval, the histogram of the bootstrap statistics, and the point estimate (calculated on the original sample).
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{ci}{The confidence interval of the statistic based on the bootstrap resampling.}
#'   \item{fun}{The statistic function used (e.g., `"mean"`).}
#'   \item{x}{The original sample data.}
#'   \item{xstat}{A vector of bootstrap sample statistics.}
#' }
#'
#' @examples
#' \dontrun{
#' # Sample data
#' sample_data <- rnorm(100, mean = 5, sd = 2)
#' # Calculate bootstrap confidence interval for the mean
#' result <- myboot2(iter = 5000, x = sample_data, fun = "mean", alpha = 0.05)
#' }
#'
#' @importFrom graphics abline hist segments text
#' @importFrom stats apply quantile sample
#' @export

myboot2 <- function(iter = 10000,
                    x,
                    fun = "mean",
                    alpha = 0.05,
                    cx = 1.5,
                    ...) {
  #Notice where the ... is repeated in the code
  n = length(x)   #sample size

  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow= n, ncol= iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun) # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para = hist(
    xstat,
    freq = FALSE,
    las = 1,
    main = paste(
      "Histogram of Bootstrap sample statistics",
      "\n",
      "alpha=",
      alpha,
      " iter=",
      iter,
      sep = ""
    ),
    ...
  )

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x,
               nrow= length(x),
               ncol= 1,
               byrow = TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")# Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4)      #Make the segment for the ci
  text(ci[1],
       0,
       paste("(", round(ci[1], 2), sep = ""),
       col = "Red",
       cex = cx)
  text(ci[2],
       0,
       paste(round(ci[2], 2), ")", sep = ""),
       col = "Red",
       cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(
    ci = ci,
    fun = fun,
    x = x,
    xstat = xstat
  ))# Some output to use if necessary
}
