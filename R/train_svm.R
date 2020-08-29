#' Output SVM model results over multiple folds
#'
#' @param folds A list of folds (training data)
#' @param kernel The SVM kernel
#' @param param A list of the parameters that can be passed to the SVM. Requires that the list names are the model's arguments.
#' @param fold_avg Default TRUE. Returns the average RMSE and R2 for the set of folds. Set as F to retain individual performance.
#' @return Either a data frame of the averaged results or a data frame of the individual performance on each fold.

train_svm <- function(folds, kernel, param, fold_avg = T) {
  svm_param <- append(list("kernel" = kernel), param)
  if (fold_avg) {
    fold_results <- do.call(
      rbind,
      mapply(
        train_svm_fold,
        fold = folds,
        name = names(folds),
        MoreArgs = list(parameters = svm_param),
        SIMPLIFY = F
      )
    )
    data.frame(
      svm_param,
      "rmse" = mean(fold_results$rmse),
      "r2" = mean(fold_results$r2)
    )
  } else {
    do.call(
      rbind,
      mapply(
        train_svm_fold,
        fold = folds,
        name = names(folds),
        MoreArgs = list(parameters = svm_param),
        SIMPLIFY = F
      )
    )
  }
}
