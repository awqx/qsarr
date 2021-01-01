#' Find outliers in a vector
#'
#' `find_outlier` returns the index of a vector where outliers occur.
#' Outliers are defined as values that exceed the number of standard
#' deviations passed to the function as a `threshold` value.
#'
#' The function`[sd()]` is used to obtain the population standard
#' deviation. This function will remove NA and NaN values.
#'
#' @param x A numeric vector of values
#' @param threshold Indices of values that are farther from the mean
#'   than the `threshold` times the  deviation are removed. The
#'   standard deviation is obtained using `[sd()]`. The default value
#'   is `threshold = 3`.
#' @return A numeric vector of the indices of outliers.
#' @export

find_outlier <- function(x, threshold = 3) {
  x_std <- (x - mean(x, na.rm = T)) / sd(x)
  which(abs(x_std) > threshold)
}
