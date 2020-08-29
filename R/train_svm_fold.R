#' A helper for \code{train_svm}.
#'
#' @param fold The fold containing the data
#' @param name The fold number
#' @param parameters A list of the parameters that can be passed to the SVM. Requires that the list names are the model's arguments.
#' @return The performance of an SVM with the listed parameters on a single fold of data.

train_svm_fold <- function(fold, name, parameters) {
  svm_param_fold <- append(
    list(
      "x" = fold$trn[, -1:-2],
      "y" = fold$trn$dG
    ),
    parameters
  )
  tst_x <- fold$tst[, -1:-2]
  tst_y <- fold$tst$dG
  svm_model <- do.call(svm, svm_param_fold)
  svm_df <- data.frame(
    "pred" = predict(svm_model, tst_x),
    "obs" = tst_y
  )
  data.frame(
    parameters,
    "fold" = name,
    "rmse" = defaultSummary(svm_df)["RMSE"],
    "r2" = defaultSummary(svm_df)["Rsquared"]
  )
}
