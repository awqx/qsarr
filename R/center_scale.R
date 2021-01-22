#' Center and scale data and remove predictors with near-zero variance
#'
#' `center_scale` preprocesses data.
#'
#' The data is centered so that predictors have a mean of 0. The data is
#' scaled so the standard deviation is 1.
#'
#' After the above cleaning steps, the mean of each descriptor should be 0.
#' The NAs in the data frame will be replaced with 0, using the assumption
#' that missing values can be estimated to be the expectation of the descriptor.
#'
#' This uses an `"ad"` S3 object constructed using `[ad()]`.
#'
#' Because the data frame to be passed to the function will likely include
#' columns that do not need to be transformed (like columns for identification)
#' or response variables, there is an option to ignore these columns using
#' `ignore_col`. The input to `ignore_col` should be a character vector.
#'
#' @param df The data frame to be processed.
#' @param ignore_col Columns that will not be preprocessed, given as a
#'   character vector. This will likely constitute the response variable.
#'   The default is `ignore_col = NA`.
#'   These columns will be reappended to the data frame at the beginning.
#' @param return_ad Whether or not to return the applicability domain object.
#'   Set this to `return_ad = T` if this is training data and the resulting
#'   applicability domain object could be useful for testing data.
#' @param ad An optional `"ad"` object to pass. Use this if you are centering
#'   and scaling test data. Default is `ad = NA` and a new object is created
#'   and discarded within the function (unless `return_ad = T`).
#' @param quiet Whether to return a message if there are columns in the original
#'   data frame that are dropped.
#' @return If `return_ad = F` (default), a data frame with columns centered
#'   and scaled.
#'   Columns can be ignored and not be preprocessed.
#'   If `return_ad = T`, a list with the data frame with columns centered
#'   and scaled as well as the `"ad"` class object.
#'   These are labeled `"df"` and `"ad"`, respectively.
#'   Note that only the columns in `ignore_col` and named in the `ad_obj` provided
#'   will be returned. Set `quiet = F` to be notified if columns are lost.
#' @importFrom tidyr replace_na
#' @export

center_scale <-
  function(df,
           ignore_col = NA,
           return_ad = F,
           ad_obj = NA,
           quiet = T,
           ...) {
  if (!is.na(ignore_col[1])) {
    ignore_index <- which(names(df) %in% ignore_col)
    df_retain <- df[, ignore_index]
    df <- df[, -ignore_index]
  }

  # Centering and scaling
  # Uses an applicability domain object with `ad()`
  if (is.na(ad_obj)[1]) {
    ad_obj <- ad(df, ignore_col = ignore_col)
  }


  if (!quiet) {
    if (ncol(df) > length(ad_obj$x_mean)) {
      message(
        "There are ", ncol(df) - length(ad_obj$x_mean), " more predictors ",
        "in the data frame compared to the applicability domain", "\n",
        "Consider ignoring columns with `ignore_col =`", "\n",
        "Only retaining predictors specified in applicability domain"
      )
    }
  }

  # Slightly different that previous error check
  # If predictors in the ad object are not present in the data frame, this is a
  # critical issue that prevents evaluation of the applicability domain
  if (sum(names(ad_obj$x_mean) %in% names(df)) < length(ad_obj$x_mean)) {
    message(
      "Predictors in the applicability domain are missing from the data", "\n",
      "Prediction cannot proceed; exiting with NA"
    )
    return(NA)
  }

  cs_df <- lapply(
    names(ad_obj$x_mean),
    function(x) (df[, x] - ad_obj$x_mean[x]) / ad_obj$x_sd[x]
  ) %>%
    data.frame() %>%
    setNames(names(ad_obj$x_mean))

  # replace all NAs with the mean (0)
  cs_df <- lapply(
    cs_df,
    function(x) {
      x[is.na(x)] <- 0
      x
    }
  ) %>%
    data.frame()

  if (return_ad) {
    list(
      df = data.frame(df_retain, cs_df),
      ad = ad_obj
    )
  } else {
    data.frame(df_retain, cs_df)
  }
}
