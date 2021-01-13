#' Remove predictors with near-zero variance
#'
#' `remove_zerovar` is a wrapper for `caret::nearZeroVar` and indexing the data
#' frame to remove the resulting predictors.
#'
#' Because the data frame to be passed to the function will likely include
#' columns that do not need to be transformed (like columns for identification)
#' or response variables, there is an option to ignore these columns using
#' `ignore_col`. The input to `ignore_col` should be a character vector.
#'
#' @param df The data frame to be processed.
#' @param ignore_col Columns that will not be preprocessed, given as a character
#'   vector. This will likely constitute the response variable. The default is
#'   `ignore_col = NA`. These columns will be reappended to the data frame at
#'   the beginning.
#' @param quiet Whether to suppress the message for how many predictors are
#'   removed in preprocessing. The default is `quiet = T`.
#' @param ... Additional arguments to be passed to `caret::nearZeroVar`
#' @return A data frame with columns centered and scaled and with variables with
#'   near-zero variance removed. Columns can be ignored and not be preprocessed.
#' @importFrom caret nearZeroVar
#' @export

remove_zerovar <- function(df, ignore_col = NA, quiet = T, ...) {
  if (!is.na(ignore_col[1])) {
    ignore_index <- which(names(df) %in% ignore_col)
    df_retain <- df[, ignore_index]
    df <- df[, -ignore_index]
  }

  param <- list(x = df, ...)
  zero_var <- do.call(nearZeroVar, param)
  df <- df[, -zero_var]

  if (!quiet) {
    message(length(zero_var), " variables had near-zero variance.")
  }
  data.frame(df_retain, df)
}
