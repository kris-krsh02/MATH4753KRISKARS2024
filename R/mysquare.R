#' Square Function
#'
#' This function computes the square of each element in a numeric vector.
#'
#' @param x A numeric vector. Each element should be a quantitative value to be squared.
#'
#' @return A numeric vector with each element squared.
#' @export
#'
#' @examples
#' # Square a vector of integers from 1 to 10
#' mysquare(x = 1:10)
#'
#' # Square a vector of real numbers
#' mysquare(x = c(0.5, 1.5, 2.5))
mysquare <- function(x) {
  x^2
}
