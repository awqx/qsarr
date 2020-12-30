#' Make an external validation set
#'
#' `make_extval` takes a data frame and splits it into a data set
#' for model tuning and training as well as another data set for
#' external validation of the model.
#'
#' @param df The data frame to split
#' @param p The proportion of data in the split. Default is `0.15.`
#' @param seed The random seed for the data. The default is the
#'   current time as an integer, i.e., `as.integer(Sys.time())`.
#' @return A list where the entry `trn` is the retained data and
#'   the entry `extval` is set aside for external validation.
#' @export

make_extval <- function(df,
                        p = 0.15,
                        seed = as.integer(Sys.time())) {
  set.seed(seed)
  ev_index <- sample(nrow(df), round(nrow(df) * p))
  list(
    trn = df[-ev_index,],
    extval = df[ev_index,]
  )
}
