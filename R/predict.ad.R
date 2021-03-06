#' Find applicability domain
#'
#' `predict.ad` takes a data frame of chemical descriptors and returns the
#' indices of the molecules that are X-outliers. The determination of outliers
#' uses a method from a 2015 paper by Roy, Kar, and Ambare that can be found
#' here: <https://doi.org/10.1016/j.chemolab.2015.04.013>.
#'
#' The first step is to standardize the values. This can be accomplished by
#' creating an `"ad"` class object using `[ad()]`. This creates a list of the
#' means and standard deviations of training data. It is important to only call
#' `ad` on training data because in model-building, the testing data should be
#' used for evaluation and not be considered in the model-building phase.
#'
#' `predict.ad` will use the information in the `"ad"` object and standardize
#' the descriptor. It will return the descriptors as centered and scaled (mean
#' of 0 and standard deviation of 1). Additionally, this will be converted to
#' absolute values. accomplished using `center_scale_zero` and returning the Let
#' the standardized value corresponding to descriptor `i` of molecule `k` be
#' referred to as `s_ik`.
#'
#' Next, the maximum deviation of each molecule needs to be found. This requires
#' examining the entries rowwise. If the maximum `s_ik` values is less than 3,
#' the molecule is not an X-outlier. If the minimum `s_ik` is greater than 3,
#' the molecules is an X-outlier. If the minimum is less than 3 and the maximum
#' is greater than 3, we recalculate `s_newk`.
#'
#' `s_newk` is given as the mean of `s_ik` values for molecule `k` added to 1.28
#' times the standard deviation of the `s_ik` values for molecule `k`. If this
#' is less than 3, then `k` is not an X-outlier.
#'
#' The function only returns a vector of booleans. To remove the X-outliers in a
#' data set and return the cleaned data frame, use the function
#' `[remove_xoutlier()]`.
#'
#' The data frame may have columns that are not chemical descriptors. This will
#' not hinder the ability to make predictions, though this behavior may not be
#' expected. By setting `msg = T`, extra columns can be detected.
#'
#' @param ad An ad object
#' @param df A data frame of chemical descriptors
#' @param msg Whether to return a message when there are more predictors in the
#'   data frame than in the applicability domain object. Typically, this will not
#'   be a problem as long as the relevant predictors are still present. The
#'   default is `msg = F`.
#' @usage predict(ad_obj, df, ignore_col = NA, ...)
#' @return An integer vector of the row indices of X-outliers
#' @export

predict.ad <- function(ad, df, msg = F, ...) {
  if (!is.na(ad$ignore_col[1])) {
    ignore_index <- which(names(df) %in% ad$ignore_col)
    if (length(ignore_index)) df <- df[, -ignore_index]
  }

  if (msg) {
    if (ncol(df) > length(ad$x_mean)) {
      message(
        "There are ", ncol(df) - length(ad$x_mean), " more predictors ",
        "in the data frame compared to the applicability domain"
      )
    }
  }

  # Slightly different that previous error check
  # If predictors in the ad object are not present in the data frame, this is a
  # critical issue that prevents evaluation of the applicability domain
  if (sum(names(ad$x_mean) %in% names(df)) < length(ad$x_mean)) {
    message(
      "Predictors in the applicability domain are missing from the data", "\n",
      "Prediction cannot proceed; exiting with NA"
    )
    return(NA)
  }

  std_df <- lapply(
    names(ad$x_mean),
    function(x) {
      x_mean <- ad$x_mean[x]
      x_sd <- ad$x_sd[x]
      abs(df[, x] - x_mean) / x_sd
    }
  ) %>%
    data.frame()

  # Apply a function rowwise
  # returns T if inside domain
  sapply(
    1:nrow(std_df),
    function(x) {
      x <- std_df[x, ] %>% as.numeric()
      if (min(x, na.rm = T) > 3) F
      if (max(x, na.rm = T) < 3) T
      s_new <- mean(x, na.rm = T) + 1.28 * sd(x, na.rm = T)
      ifelse(s_new < 3, T, F)
    }
  )
}
