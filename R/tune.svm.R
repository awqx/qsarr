#' Tune an SVM with linear kernel
#'
#' For all SVM methods, the function is tuned on `[e1071::svm()]` and assumes
#' that the SVM type being used for model-building is `"eps-regression"`. This
#' assumes that the response variable being passed to the function is numeric.
#' The list of parameters to tune can be found in documentation for the function
#' `?e1071::svm`. The methods `"svm_linear"`, `"svm_polynomial"`,
#' `"svm_radial"`, and `"svm_sigmoid"` are separated because each SVM kernel can
#' take different combinations of parameters to tune.
#'
#' Possible parameters to tune `"svm_linear"` include `cost`, `tolerance`, and
#' `epsilon`.
#'
#' Possible parameters to tune `"svm_polynomial"` include `degree`, `gamma`,
#' `coef0`, `cost`, `tolerance`, and `epsilon`.
#'
#' Possible parameters to tune `"svm_radial"` include `gamma`, `cost`,
#' `tolerance`, and `epsilon`.
#'
#' Possible parameters to tune `"svm_sigmoid"` include `gamma`, `coef0`, `cost`,
#' `tolerance`, and `epsilon`.
#'
#' @importFrom e1071 svm
#' @usage tune(method = "svm_linear", df, resp, nfold = 10, nrep = 1, ...)
#' @rdname tune
#' @examples # Using "svm_linear" as the method
#' tune(
#'   method = "svm_linear", df = your_data, resp = "y",
#'   nfold = 10, nrep = 10,
#'   cost = c(0, 0.1, 0.25, 0.5, 1),
#'   epsilon = c(0, , 0.05, 0.1, 0.5, 1),
#' )
#' @export

tune.svm_linear <- function(method,
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
      data = df,
      kernel = "linear"
    ),
    after = 0
  )

  final_model <- do.call(svm, final_param)

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

#' @rdname tune
#' @usage tune(method = "svm_polynomial", df, resp, nfold = 10, nrep = 1, ...)
#' @export

tune.svm_polynomial <- function(method,
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
      data = df,
      kernel = "polynomial"
    ),
    after = 0
  )

  final_model <- do.call(svm, final_param)

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

#' @rdname tune
#' @usage tune(method = "svm_radial", df, resp, nfold = 10, nrep = 1, ...)
#' @export

tune.svm_radial <- function(method,
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
      data = df,
      kernel = "radial"
    ),
    after = 0
  )

  final_model <- do.call(svm, final_param)

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

#' @rdname tune
#' @usage tune(method = "svm_sigmoid", df, resp, nfold = 10, nrep = 1, ...)
#' @export

tune.svm_sigmoid <- function(method,
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
      data = df,
      kernel = "sigmoid"
    ),
    after = 0
  )

  final_model <- do.call(svm, final_param)

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
