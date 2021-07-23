#' Evaluate a model using k-fold cross-validation
#'
#' `eval_model` uses k-fold cross-validation to assess the performance of a type
#' of model on a dataset.
#'
#' Currently, the function can evaluate the following model types, passed
#' through the parameter `method`:
#'
#' * `"rf"`: Random Forest from the package `randomForest`
#' * `"svm_linear"`: SVM with linear kernel from the package `e1071`
#' * `"svm_polynomial"`: SVM with polynomial kernel
#' * `"svm_radial"`: SVM with radial basis function kernel
#' * `"svm_sigmoid"`: SVM with sigmoid kernel
#' * `"earth"`: MARS with package `earth`
#'
#' @param df The data frame to train the model on
#' @param resp The name of the column to be used as a response variable.
#' @param method The method to be used in model-building. See the description
#'   for available methods.
#' @param ignore_col Columns that will not be used in model-building, given as a
#'   character vector. This may be an identifying column. The default is
#'   `ignore_col = NA`.
#' @param nfold The number of folds to use in k-fold cross-validation. The
#'   default is `nfold = 10`.
#' @param simplify Whether to return the results from all folds or return a data
#'   frame summarizing the results (only reporting the average and standard
#'   deviation of all folds). The default is `simplify = T`.
#' @param ... Additional arguments to pass to the model method
#' @return A data frame of the results of k-fold cross-validation with the
#'   specified model parameters. Can be unsimplified (`simplify = F`), returning
#'   the result on each fold, or simplified, returning the average and standard
#'   deviation only. The function uses `caret::defaultSummary` and provides the
#'   summary statistics of R-squared, RMSE, and MAE.
#' @importFrom caret defaultSummary createFolds
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom randomForest randomForest
#' @importFrom e1071 svm
#' @export

eval_model <- function(df,
                       resp = NA,
                       method,
                       nfold = 10,
                       simplify = T,
                       ignore_col = NA,
                       ...) {
  if (is.na(resp)) {
    message("Specify a column name to be used as a response variable")
    return(0)
  }

  if (!resp %in% colnames(df)) {
    message("Column name for response is not in the data frame")
    return(0)
  }

  if (missing(method)) {
    message(
      "Specify a method for model-building.", "\n",
      "See `?eval_model` for available options for methods."
    )
    return(0)
  }

  # Using indexing because it handles removal of columns better
  ignore_index <- which(names(df) %in% ignore_col)
  # This control statement prevents errors if `ignore_col = NA`
  if (length(ignore_index)) {
    df <- df[, -ignore_index]
  }

  # caret::createFolds should be given a vector
  fold_index <- createFolds(y = df[, resp], k = nfold)

  result <- lapply(
    fold_index,
    function(ind) {
      trn <- df[-ind, ]
      tst <- df[ind, ]
      # Using a formula is preferred because most model-building methods will
      # accept formulae but some may not accept a response vector alone
      param <- list(
        formula = as.formula(paste0(resp, " ~ .")),
        data = trn,
        ...
      )

      model <- switch(
        method,
        "rf" = do.call(randomForest, param),
        "svm_linear" = {
          param <- append(param, "linear")
          names(param)[length(param)] <- "kernel"
          do.call(svm, param)
        },
        "svm_polynomial" = {
          param <- append(param, "polynomial")
          names(param)[length(param)] <- "kernel"
          do.call(svm, param)
        },
        "svm_sigmoid" = {
          param <- append(param, "sigmoid")
          names(param)[length(param)] <- "kernel"
          do.call(svm, param)
        },
        "svm_radial" = {
          param <- append(param, "radial")
          names(param)[length(param)] <- "kernel"
          do.call(svm, param)
        },
        "earth" = do.call(earth, param),
        NA
      )

      if (is.na(model[1])) {
        message(
          "Model-building failed.", "\n",
          "See `?eval_model` for available options for methods."
        )
        return(0)
      }

      pred_y <- predict(model, tst)
      pred_df <- data.frame(tst[resp], pred_y)
      # caret::defaultSummary requires specific names for the columns
      colnames(pred_df) <- c("obs", "pred")

      defaultSummary(pred_df)
    }
  ) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    rownames_to_column(var = "fold") %>%
    pivot_longer(!fold, names_to = "summary_stat")

  if (simplify) {
    result <- result %>%
      group_by(summary_stat) %>%
      summarize(
        fold_avg = mean(value, na.rm = T),
        fold_sd = sd(value, na.rm = T)
      ) %>%
      data.frame()
  }

  result
  # result
}
