#' Remove rows of a data frame based on column outliers
#'
#' `remove_col_outlier` removes rows in a data frame based on the
#' outliers in a column.
#' It is a wrapper for `[find_col_outlier()]`.
#'
#' @param df The data frame to search through
#' @param col The name or index of the column in the data frame.
#' @param threshold Indices of values that are farther from the mean
#'   than the `threshold` times the  deviation are removed. The
#'   standard deviation is obtained using `[sd()]`. The default value
#'   is `threshold = 3`.
#' @param quiet Whether to print a message on the number of outliers
#'   removed. The default is `quiet = F`.
#' @return A data frame with rows containing outliers in a specified
#'   column removed.
#' @export

remove_col_outlier <- function(df, col, threshold = 3, quiet = T) {
  outlier <- find_col_outlier(df = df, col = col, threshold = threshold)
  if (!quiet) {
    message(
      "There are ", length(outlier), " outliers ",
      "in the column ", col)
  }
  if (!length(outlier)) return(df)
  df[-outlier, ]
}
