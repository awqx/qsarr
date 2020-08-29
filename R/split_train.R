#' Create \code{num} splits in training data saved in a single list
#'
#' @param num The number of folds created
#' @param seed The random seed, for reproducibility
#' @return A list containing \code{num} folds of the training set data

split_train <- function(trn, num, seed) {
  set.seed(seed)
  fold_list <- createFolds(trn$dG, k = num)
  lapply(
    fold_list,
    function(x, dat) list("trn" = dat[-x, ], "tst" = dat[x, ]),
    dat = trn
  )
}
