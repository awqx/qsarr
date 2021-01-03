#' Replace NaNs in a data frame with NA
#'
#' `replace_nan` replaces the not-a-number values in a dataframe with `NA`s.
#' This makes operations that default to using `na.rm` or similar arguments
#' easier to troubleshoot.
#'
#' @param df The data frame where values will be replaced
#' @return A data frame with `NA` in place of `NaN`
#' @export

replace_nan <- function(df) {
  lapply(
    df,
    function(x) {
      x[which(is.nan(x))] <- NA
      x
    }
  ) %>%
    data.frame()
}
