#' Remove columns based on proportion of NA values
#'
#' `remove_col_na` removed columns in a data frame where the proportion of
#' `NA` values exceeds the given threshold.
#'
#' @param df The data frame where columns are removed
#' @param threshold The maximum allowable proportion of `NA`.
#' The default is `threshold = 0.2`
#' @param quiet Whether to suppress the display the number of removed columns.
#' The default is `quiet = T`.
#' @return A data frame where columns whose proportion of `NA`s exceed a given
#' proportion removed
#' @export

remove_col_na <- function(df, threshold = 0.2, quiet = T) {
  retain_col <- sapply(
    df,
    function(x) {
      mean(is.na(x)) <= threshold
    }
  )
  if (!quiet) {
    message(sum(retain_col), " column(s) were removed.")
  }
  df[, retain_col]
}
