#' Calculate Total Sum of Squares (TSS)
#'
#' This function calculates the Total Sum of Squares (TSS) for a specified column in a data frame.
#' TSS is the sum of squared deviations from the mean of the specified column.
#'
#' @param data A data frame containing the data.
#' @param column A string specifying the name of the column for which to calculate TSS.
#'               The column should contain numeric values.
#'
#' @return A numeric value representing the Total Sum of Squares (TSS).
#' @export
#'
#' @examples
#' # Calculate TSS for the "Sepal.Length" column in the iris dataset
#' calculateTSS(iris, "Sepal.Length")
calculateTSS <- function(data, column) {
  with(data, sum((get(column) - mean(get(column)))^2))
}
