#' Find outliers in a column of a data frame
#'
#' `find_col_outlier` finds the outliers in a column of a data frame.
#' It is a wrapper for `[find_outlier()]`.
#'
#' @param df The data frame to search through
#' @param col The name or index of the column in the data frame.
#' @param threshold Indices of values that are farther from the mean
#'   than the `threshold` times the  deviation are removed. The
#'   standard deviation is obtained using `[sd()]`. The default value
#'   is `threshold = 3`.
#' @return A vector of indices where outliers occur
#' @export

find_col_outlier <- function(df, col, threshold = 3) {
  find_outlier(x = df[, col], threshold = threshold)
}
