#' Find the best performing model in a list
#'
#' `best_summary_stat` returns the index of an element in a list of `"tune"`
#' objects with the best combination of performance statistics.
#'
#' This function is a helper for `[tune_helper()]`.
#'
#' The function relies on `[caret::defaultSummary()]`. It calls on
#' `normalize_summary_stat`, which rescales the summary statistics of the list
#' of objects so all statistics given to the function can be evaluated together.
#'
#' @param tune_list A list of `"tune"` objects
#' @param stat_name_list A character vector of which statistics to consider. The
#'   default is to consider all statistics, provided through the character vector
#'   `c("MAE", "Rsquared", "RMSE")`.
#' @return An integer index of the list where the best model occurs. Uses
#'   `which.max`, so if multiple maximums occur, the first will be returned.
#'   If there are issues with finding the maximum, the default return is `1`.

normalize_summary_stat <- function(tune_list, stat_name) {
  if (!stat_name %in% c("MAE", "Rsquared", "RMSE")) {
    message(
      "Only options for summary statistics are ",
      "MAE, Rsquared, or RMSE")
    return()
  }

  raw_val <- sapply(
    tune_list,
    function(x) {
      x <- x$result_summary
      x[x$summary_stat == stat_name, "fold_avg"]
    }
  )

  val_range <- range(raw_val, na.rm = T)
  if (stat_name == "Rsquared") {
    (raw_val - val_range[1])/(val_range[2] - val_range[1])
  } else {
    (val_range[2] - raw_val)/(val_range[2] - val_range[1])
  }
}

best_summary_stat <-
  function(tune_list,
           stat_name_list = c("MAE", "Rsquared", "RMSE")) {
    sum_list <- lapply(stat_name_list,
                       normalize_summary_stat,
                       tune_list = tune_list) %>%
      do.call(rbind, .) %>%
      colSums()
    result <- which.max(sum_list) %>% unlist()
    if (!length(result)) return(1)
    result
  }
