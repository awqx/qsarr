#' Remove X-outliers from data
#'
#' `remove_xoutlier` is a wrapper for `[predict.ad()]` and indexing the
#' data frame to retain values that are not X-outliers.
#'
#' Additional arguments that may be useful to pass are `ignore_col = `
#' to specify columns to ignore in building the applicability domain
#' object using `[ad()]` inside the function.
#'
#' `remove_xoutlier` is similar to `[remove_ad()]`. The latter function
#' is for removing observations outside the applicability domain of the
#' QSAR for testing data. It requires an additional argument `ad` that
#' specifies the `"ad"` class object to define the applicability domain
#' of the training data.
#'
#' @param df The data frame where X-outliers will be removed. This
#'   should be training data only.
#' @param quiet Whether to message how many observations were found
#'   to be X-outliers. The default is `quiet = T`.
#' @return A data frame with X-outliers removed.
#' @export

remove_xoutlier <- function(df, quiet = T, ...) {
  ad_obj <- ad(df, ...)
  outlier_index <- predict(ad_obj, df, ...)
  if (!quiet) {
    message(sum(!outlier_index), " X-outliers removed")
  }
  df[outlier_index, ]
}
