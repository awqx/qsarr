#' Print an object of S3 class "tune"
#'
#' Calling `print` on a `"tune"` object provides details on the model type and
#' the model performance.
#'
#' @param tune_obj The object to be printed
#' @export

print.tune <- function(tune_obj) {
  cat(
    "Model type: ", class(tune_obj$model), "\n\n",
    "Tuned parameters: ", "\n",
    sapply(
      names(tune_obj$param),
      function(x) {
        paste0("\t", x, ": ", tune_obj$param[[x]], "\n")
      }
    ), "\n",
    "Number of predictors: ", length(tune_obj$pred_name), "\n",
    "Predictors (first 10): ",
    tune_obj$pred_name[1:(min(10, length(tune_obj$pred_name)))],
    "\n", "\n",
    "Model performance: ", "\n"
  )
  print(tune_obj$result_summary)
}
