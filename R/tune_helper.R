#' A helper function for `tune`
#'
#' `tune_helper` is a helper function that summarizes part of the tuning process
#' for the method `tune`.
#'
#' Calls on `best_summary_stat` to find the best-performing model.
#'
#' @param method The model-building method
#' @param df The data frame to train on
#' @param resp The name of the column containing the response variable
#' @param nfold The number of folds to use in evaluation
#' @param nrep The number of repetitions to use in evaluation
#' @param ignore_col Columns to ignore during model-building
#' @return A list containing several fields relevant for the model:
#' * `$param`: The parameters used in the best model
#' * `$result_summary`: The summarized results for the best model
#' * `$result_all`: The results for each repetition for the best model
#' * `$best_index`: The index of the best model relative to `$all_tune`
#' * `$all_tune`: The list of all parameters used in tuning

tune_helper <- function(method,
                        df,
                        resp,
                        nfold,
                        nrep,
                        ignore_col,
                        ...) {
  # obtaining all combinations of parameters
  tune_grid <- expand.grid(list(...), stringsAsFactors = F)

  tune_grid_list <- split(tune_grid, seq(nrow(tune_grid)))
  names(tune_grid_list) <- NULL

  # Use eval_model_rep to evaluate performance
  tune_list <- lapply(
    tune_grid_list,
    function(x) {
      param <- append(
        x,
        list(
          df = df,
          resp = resp,
          method = method,
          nfold = nfold,
          nrep = nrep,
          ignore_col = ignore_col
        )
      )
      do.call(eval_model_rep, param)
    }
  )

  # Remove names to prevent odd indexing errors
  names(tune_list) <- NULL
  param_list <- Map(
    function(x, y) {
      list(
        param = x,
        result = y$result_summary
      )
    },
    x = tune_grid_list,
    y = tune_list
  )

  tune_index <- best_summary_stat(tune_list = tune_list)

  list(
    param = tune_grid_list[[tune_index]],
    result_summary = tune_list[[tune_index]]$result_summary,
    result_all = tune_list[[tune_index]]$result_all,
    best_index = tune_index,
    all_tune = param_list
  )
}
