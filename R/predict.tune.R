#' Make predictions using the S3 "tune" object
#'
#' `predict.tune()` is a wrapper for using predict on the object stored under
#' the name `"model"` in the object.
#'
#' @param tune_obj The object for prediction
#' @param ... Additional arguments to be passed to `predict`
#' @export

predict.tune <- function(tune_obj, ...) {
  # if (length(class(tune_obj$model)) > 1) {
  #   class(tune_obj$model) <-
  #     class(tune_obj$model)[length(class(tune_obj$model))]
  # }
  # best_model <- tune_obj$model
  # UseMethod("predict", best_model)
  predict(tune_obj$model, ...)
}
