#' Tune parameters of a model-building method
#'
#' `tune` evaluates model performance on a combination of parameters. The methods
#' available are the same as `[eval_model()]`.
#'
#' Calling `print` on a `"tune"` object provides details on the model type and
#' the model performance.
#'
#' Calling `predict` on a `"tune"` object runs prediction using the class of the
#' model stored in the object.
#'
#' @param method The method to be used in model-building. See the description
#'   for `[eval_model()]` for available methods.
#' @param ... Additional arguments to be passed to model-building. This will
#'   likely be vectors of the values of the parameters to test.
#' @return An object of the S3 class `"tune"`. Includes a list of the model with
#'   the best performing parameters.
#' * `$model`: the final model with the tuned parameters
#' * `$param_tested`: a list of the parameters used in the tuning process
#' * `$nfold_tested`: the number of folds in each iteration of tuning
#' * `$nrep_tested`: the number of repetitions in each iteration of tuning
#' * `$pred_name`: the predictors from the data set
#' @export

tune <- function(method, ...) {
  if (missing(method)) {
    messages(
      "Specify a method for model-building", "\n",
      "See `?eval_model` for available options for methods"
    )
    return()
  }
  dummy <- 1
  class(dummy) <- switch(
    method,
    "rf" = "randomForest",
    "svm_linear" = "svm_linear",
    "svm_polynomial" = "svm_polynomial",
    "svm_sigmoid" = "svm_sigmoid",
    "svm_radial" = "svm_radial",
    "earth" = "earth",
    "mars" = "earth",
    "glm" = "glm",
    "glm" = "glmnet",
    "unavailable_model"
  )
  UseMethod("tune", dummy)
}
