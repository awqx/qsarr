#' Tune a random forest
#'
#' `tune.randomForest` tests the function `[randomForest::randomForest()]` with
#' provided parameters.
#'
#' @param method The model-building method. Should be `"rf"` at this point.
#' @param df The data frame to train on
#' @param resp The name of the column containing the response variable
#' @param nfold The number of folds to use in evaluation. Default is `10`.
#' @param nrep The number of repetitions to use in evaluation. Default is `1`.
#' @param ignore_col Columns to ignore during model-building. Default is `NA`.
#' @importFrom randomForest randomForest
#' @return An object of S3 class `"tune"`.
#' @export

tune.randomForest <- function(method,
                              df,
                              resp,
                              nfold = 10,
                              nrep = 1,
                              ignore_col = NA,
                              ...) {
  if (missing(resp) || missing(df)) {
    message("Specify a response variable and data frame")
    return()
  }

  tune_obj <- tune_helper(
    method = method,
    df = df,
    resp = resp,
    nfold = nfold,
    nrep = nrep,
    ignore_col = ignore_col,
    ...
  )

  ignore_index <- which(names(df) %in% ignore_col)
  if (length(ignore_index)) {
    df <- df[,-ignore_index]
  }

  final_param <- append(
    tune_obj$param,
    list(
      formula = as.formula(paste0(resp, " ~ .")),
      data = df
    ),
    after = 0
  )

  final_model <- do.call(randomForest, final_param)

  obj <- append(
    tune_obj,
    list(
      model = final_model,
      param_tested = list(...),
      nfold_tested = nfold,
      nrep_tested = nrep,
      pred_name = names(df)[names(df) != resp]
    )
  )
  class(obj) <- "tune"
  obj
}
