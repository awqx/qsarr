#' Use the predict method
#'
#' `predict` executes model prediction on a variety of objects.
#'
#' Calling `predict` on a `"tune"` object uses the model to predict new values.
#'
#' Calling `predict` on a `"ad"` object sorts out observations outside of the
#' applicability domain.
#'
#' @param method The method to be used in model prediction. See the description
#'   for `[eval_model()]` for available methods.
#' @param ... Additional arguments to be passed to model-building. This will
#'   likely be vectors of the values of the parameters to test.
#' @return Predicted values
#' @export

function(object, ...) {
  UseMethod("predict")
}
