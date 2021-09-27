#' Tune GLM
#'
#' Passing `method = "glm"` or `method = "glmnet"` tunes a GLM using the
#' function `[glmnet::glmnet()]`.
#'
#' There are many parameters to tune the GLM models. Likely the most useful ones will
#' be `alpha`, `nlambda`, `dfmax`, `pmax`, and `family`.
#'
#' An `alpha` value of `alpha = 1` uses lasso penalty. An `alpha = 0` uses ridge penalty.
#'
#' @importFrom glmnet glmnet
#' @usage # To call GLM (methods are identical)
#'   tune(method = "glm", df, resp, nfold = 10, nrep = 1, ...)
#'   tune(method = "glmnet", df, resp, nfold = 10, nrep = 1, ...)
#' @rdname tune
#' @examples # Using "mars" or "earth" as the method
#' tune(
#'   method = "earth", df = your_data, resp = "y",
#'   nfold = 10, nrep = 10,
#'   alpha = seq(0, 1, by = 0.2),
#'   fast.k = c(0, 5, 10, 20),
#'   nlambda = c(20, 50, 100, 200),
#'   dfmax = c(10, 50, length(data) - 1),
#'   pmax = c(10, 50, 100, length(data) - 1)
#' )
#' @export

tune.glm <- function(method,
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

  final_model <- do.call(glmnet, final_param)

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
