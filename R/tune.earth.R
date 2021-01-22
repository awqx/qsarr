#' Tune MARS
#'
#' Passing `method = "mars"` or `method = "earth"` tunes a MARS model using the
#' function `[earth::earth()]`.
#'
#' There are many parameters to tune `"earth"`. Likely the most useful ones will
#' be `fast.k`, `fast.beta`, `newvar.penalty`, `penalty`, `minspan`, and
#' `degree`. If time allows, `earth` can do more thorough variable selection with
#' different pruning methods and cross-validation.
#'
#' @importFrom earth earth
#' @usage # To call MARS (methods are identical)
#'   tune(method = "earth", df, resp, nfold = 10, nrep = 1, ...)
#'   tune(method = "mars", df, resp, nfold = 10, nrep = 1, ...)
#' @rdname tune
#' @examples # Using "mars" or "earth" as the method
#' tune(
#'   method = "earth", df = your_data, resp = "y",
#'   nfold = 10, nrep = 10,
#'   fast.k = c(0, 5, 10, 20),
#'   fast.beta = c(0, 1),
#'   newvar.penalty = c(0, 0.01, 0.1, 0.2, 0.25),
#'   penalty = c(2, 3, 4),
#'   minspan = c(0, 1, 4, 10)
#'   degree = c(1, 2, 3)
#' )
#' @export

tune.earth <- function(method,
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

  final_model <- do.call(earth, final_param)

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
