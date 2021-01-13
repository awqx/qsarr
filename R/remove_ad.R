#' Remove observations outside of the applicability domain
#'
#' `remove_ad` is a wrapper for `[predict.ad()]` and indexing the data frame to
#' retain values that are not X-outliers. It is similar to `[remove_xoutlier()]`
#' but is specified for testing data and not training data.
#'
#' A recommended optional argumet is `ignore_more = ` to specify a vector of
#' column indices or column names (as characters) to skip when using
#' `[predict.ad()]` in the function.
#'
#' @param df The data frame where molecules outside the applicability domain
#'   should be removed. This should be testing data only.
#' @param ad_obj An `"ad"` class object built off of the training data for the
#'   model being examined.
#' @param quiet Whether to message how many observations were found to be
#'   outside the applicability domain. The default is `quiet = T`.
#' @return A data frame with observations within the appliability domain of the
#'   training data.
#' @export

remove_ad <- function(df, ad_obj, quiet = T, ...) {
  outlier_index <- predict(ad_obj, df, ...)
  if (!quiet) {
    message(
      sum(!outlier_index),
      " observations outside the applicability domain"
      )
  }
  df[outlier_index, ]
}
