#' Obtain standard deviation
#'
#' `sd_pop` retrieves the population standard deviation of a vector of observations.
#' The function will automatically remove NA values.
#'
#' @param x Vector of numeric values
#' @param quiet Whether to print a message if NAs or NaNs exist
#' @return A numeric value corresponding to the population standard
#'   deviation of the input.
#' @export

sd_pop <- function(x, quiet = T) {
  if (sum(is.na(x))) {
    if (!quiet) message("Removing NAs when finding std. deviation")
    x <- x[!is.na(x)]
  }

  if (sum(is.nan(x))) {
    if (!quiet) message("Removing NaNs when finding std. deviation")
    x <- x[!is.nan(x)]
  }

  sd <- sum((x - mean(x))^2)
  sqrt(sd / length(x))
}
