#' Bootstrap-Based p-Value Calculation
#'
#' This function calculates the p-value using a bootstrap approach for comparing two samples.
#' It centers the data under the null hypothesis and performs resampling to approximate the
#' distribution of the test statistic.
#'
#' @param x1 Numeric vector of data from sample 1.
#' @param x2 Numeric vector of data from sample 2.
#' @param conf.level Confidence level for the test (default: 0.95).
#' @param iter Number of bootstrap iterations (default: 3000).
#' @param mudiff Difference in means under the null hypothesis (default: 0).
#' @param test Type of test to perform: `"two"` for two-tailed, `"upper"` for upper-tailed, or `"lower"` for lower-tailed (default: `"two"`).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{pvalue}{The computed p-value for the test.}
#' }
#' @importFrom graphics hist
#' @importFrom stats var
#' @examples
#' # Example usage:
#' set.seed(123)
#' x1 <- rnorm(10, mean = 5, sd = 2)
#' x2 <- rnorm(10, mean = 6, sd = 2)
#' boot2pval(x1, x2)
#'
#' @export
boot2pval <- function(x1,
                      x2,
                      conf.level = 0.95,
                      iter = 3000,
                      mudiff = 0,
                      test = "two") {
  n1 <- length(x1)
  n2 <- length(x2)
  y1 <- x1 - mean(x1) + mean(c(x1, x2))  # Transform the data so it is centered at the NULL
  y2 <- x2 - mean(x2) + mean(c(x1, x2))

  y1rs.mat <- matrix(NA, nrow = n1, ncol = iter)
  y2rs.mat <- matrix(NA, nrow = n2, ncol = iter)

  for (i in seq_len(iter)) {
    y1rs.mat[, i] <- sample(y1, n1, replace = TRUE)
    y2rs.mat[, i] <- sample(y2, n2, replace = TRUE)
  }

  x1rs.mat <- y1rs.mat + mean(x1) - mean(c(x1, x2))
  x2rs.mat <- y2rs.mat + mean(x2) - mean(c(x1, x2))

  xbar1 <- mean(x1)
  xbar2 <- mean(x2)
  sx1sq <- var(x1)
  sx2sq <- var(x2)

  tcalc <- (xbar1 - xbar2 - mudiff) / sqrt(sx1sq / n1 + sx2sq / n2)

  sy1sq <- apply(y1rs.mat, 2, var)
  sy2sq <- apply(y2rs.mat, 2, var)
  y1bar <- apply(y1rs.mat, 2, mean)
  y2bar <- apply(y2rs.mat, 2, mean)

  tstat <- (y1bar - y2bar - mudiff) / sqrt(sy1sq / n1 + sy2sq / n2)

  alpha <- 1 - conf.level
  pvalue <- switch(
    test,
    "two" = mean(abs(tstat) >= abs(tcalc)),
    "upper" = mean(tstat >= tcalc),
    "lower" = mean(tstat <= tcalc)
  )

  h <- hist(tstat, plot = FALSE)
  mid <- h$mid
  if (test == "two") {
    ncoll <- sum(mid <= -abs(tcalc))
    ncolr <- sum(mid >= abs(tcalc))
    col <- c(rep("Green", ncoll),
             rep("Gray", length(mid) - ncoll - ncolr),
             rep("Green", ncolr))
  }

  hist(tstat, col = col, freq = FALSE)

  return(list(pvalue = pvalue))
}
