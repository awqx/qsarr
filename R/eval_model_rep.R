#' Evaluate a model with replication
#'
#' `eval_model_rep` is similar to `[eval_model()]` but it allows for multiple
#' evaluations. Though it is mostly a wrapper for `[eval_model()]` and
#' `replicate`, the function adds additional ability to analyze results by
#' returning the standard deviation of model performance across replications
#' in addition to standard deviation across folds.
#'
#' To make results reproducible, set a random seed before `eval_model_rep` using
#' `set.seed`.
#'
#' @param nrep The number of replications to perform.
#' @param ... Additional arguments to pass to `[eval_model()]`.
#' @return A list with two arguments: `result_summary`, containing the results as
#'   summarized by fold and replication, and `result_all`, containing the results
#'   summarized by replication only.
#' @export

eval_model_rep <- function(nrep = 10, ...) {
  param <- list(..., simplify = F)
  rep_list <- replicate(
    nrep,
    do.call(eval_model, param),
    simplify = F
  )

  # Label each replication with its number so data can be collapsed
  rep_list <- lapply(
    1:length(rep_list),
    function(x) {
      rep_list[[x]] %>%
        mutate(nrep = x)
    }
  )

  df_all <- do.call(rbind, rep_list)

  # Create summary of all folds, regardless of replication
  df1 <- df_all %>%
    group_by(summary_stat) %>%
    summarize(
      fold_avg = mean(value, na.rm = T),
      fold_sd = sd_pop(value),
      .groups = "drop"
    ) %>%
    data.frame()

  # Create summary the divides based on replication
  # The standard deviation between folds is interesting for evaluation
  df2 <- df_all %>%
    group_by(summary_stat, nrep) %>%
    summarize(
      x = mean(value, na.rm = T),
      .groups = "drop"
    ) %>%
    group_by(summary_stat) %>%
    summarize(
      # Not includeing rep_avg = mean(x); it is identical to fold_avg
      rep_sd = sd_pop(x),
      .groups = "drop"
    ) %>%
    data.frame()

  df_summary <- inner_join(df1, df2, by = "summary_stat")

  # An additional data frame without collapsing the folds
  # Can be useful for ANOVA analysis, etc., later
  df3 <- df_all %>%
    group_by(summary_stat, nrep) %>%
    summarize(
      avg = mean(value, na.rm = T),
      sd = sd_pop(value),
      .groups = "drop"
    ) %>%
    data.frame()

  # Returns a list
  list(result_summary = df_summary, result_all = df3)
}
