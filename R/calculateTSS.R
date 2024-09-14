#' A function to calculate TSS (total sum of squares)
#'
#' @param data a data frame
#' @param column name of the column to calculate TSS for
#'
#' @return an integer value of TSS
#' @export
#'
#' @examples
#' calculateTSS(iris, "Sepal.Length")
calculateTSS <- function(data, column) {
  with(data, sum((get(column) - mean(get(column)))^2))
}
