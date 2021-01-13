#' Build a model with feature selection and tuning
#'
#' `build_model` integrates `caret`'s recursive feature elimination with tuning
#' from `[tune()]`.
#'
#' This tends to be very time-consuming. It is only implemented for `"rf"`. It
#' is suggested to run recursive feature elimination for all model methods once
#' and use those predictors for future model-building.
#'
#' @param model_method The method used. See `[eval_model()]` for options.
#' @param ... Additional arguments to pass to model-building. Includes the
#'   parameters to test as well as details for resampling. Can also include
#'   controls for `[caret::rfe()]` passed as objects created from
#'   `[caret::rfeControl()]` to the argument `rfe_ctrl`.
#' @return An object of S3 class `"tune"`. See `[tune()]` for more details.

build_model <- function(model_method, ...) {
  dummy <- 1
  class(dummy) <- switch(
    model_method,
    "rf" = "randomForest",
    "randomForest" = "randomForest"
  )
  UseMethod("build_model", dummy)
}

build_model.randomForest <- function(df,
                                     resp,
                                     nfold = 10,
                                     nrep = 1,
                                     ignore_col = NA,
                                     rfe_ctrl,
                                     rfe_subset,
                                     ...) {
  if (!is.na(ignore_col[1])) {
    ignore_index <- which(names(df) %in% ignore_col)
    if(length(ignore_index)) {
      df <- df[, -ignore_index]
    }
  }


  # RFE
  if (missing(rfe_subset)) {
    rfe_subset <- c(2^(0:6))
    rfe_subset <- rfe_subset[rfe_subset < ncol(df)]
  }

  # Create a generic rfeControl object if not provided
  if (missing(rfe_ctrl)) {
    rfe_ctrl <- rfeControl(
      functions = rfFuncs,
      method = "repeatedcv",
      repeats = 10)
  }

  rfe_obj <- rfe(
    as.formula(paste0(resp, " ~ .")),
    data = df,
    rfeControl = rfe_ctrl,
    sizes = rfe_subset
  )

  rfe_pred <- rfe_obj$optVariables
  df2 <- select(df, all_of(c(resp, rfe_pred)))

  tune(
    method = "rf",
    df = df2,
    resp = "dG",
    nfold = nfold,
    nrep = nrep,
    ignore_col = ignore_col,
    ...
  )
}
