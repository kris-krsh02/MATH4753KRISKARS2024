#' #' Plot a Normal Curve and Calculate the Area Under the Curve
#'
#' This function plots a normal distribution curve based on the specified mean (`mu`) and
#' standard deviation (`sigma`), shades the area under the curve from -∞ to `a`, and
#' calculates the probability P(X ≤ a) for the given normal distribution.
#'
#' @param mu The mean of the normal distribution.
#' @param sigma The standard deviation of the normal distribution.
#' @param a The value up to which the area under the curve is shaded and the probability is calculated.
#'
#' @return A list containing the mean (`mu`), standard deviation (`sigma`), and the
#' area (probability P(X ≤ a)) under the curve.
#'
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @examples
#' # Plot the curve for a normal distribution with mean = 10, sigma = 5, and find the
#' # area (probability) up to x = 6
#' myncurve(mu = 10, sigma = 5, a = 6)
#'
#' @export
#'
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  x <- seq(mu - 4*sigma, a, length=1000)
  y <- dnorm(x, mean=mu, sd=sigma)
  polygon(c(mu - 4*sigma, x, a), c(0, y, 0), col="Lightblue")
  area <- pnorm(a, mean=mu, sd=sigma)
  text(x=mu, y = mean(y), paste("Area = ", round(area, 4), sep=""))
  list(mu = mu, sigma = sigma, area = area)
}
