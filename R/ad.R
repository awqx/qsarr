#' Constructor for "ad" class
#'
#' `ad` creates an object of the class `"ad"`, which can be used to
#' help in defining the applicability domain of a model or to
#' determine X-outliers.
#'
#' `ad` should be called on a data frame of training values.
#' Columns to be ignored should be passed to the object as well
#' (this will likely include the response variable or any other
#' identifiers that are not relevant in computation.)
#'
#' @param df The data frame to build an applicability domain from
#' @param ignore_col Columns that will not be considered in the
#'   applicability domain, given as a character vector.
#'   This will likely constitute the response variable.
#'   The default is `ignore_col = NA`.
#'   These columns will be noted in the returned `"ad"` object.
#' @usage ad(df, ignore_col = NA)
#' @return An object of the S3 class `"ad"`. See `[find_ad()]`
#'   for usage of this object.
#' @export

ad <- function(df, ignore_col = NA) {
  if (!is.na(ignore_col[1])) {
    ignore_index <- which(names(df) %in% ignore_col)
    if (length(ignore_index)) df <- df[, -ignore_index]
  }

  if (sum(!sapply(df, is.numeric))) {
    message(
      "Error: non-numeric columns passed", "\n",
      "Coerce data frame or specify columns to ignore"
    )
    return()
  }

  obj <- list(
    x_mean = sapply(df, mean, na.rm = T),
    x_sd = sapply(df, sd, na.rm = T),
    ignore_col = ignore_col
  )

  class(obj) <- "ad"
  obj
}
