split_train <- function(trn, nfolds, seed) {
  set.seed(seed)
  fold_list <- createFolds(trn$dG, k = nfolds)
  lapply(
    fold_list,
    function(x, dat) list("trn" = dat[-x, ], "tst" = dat[x, ]),
    dat = trn
  )
}
