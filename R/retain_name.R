#' Retain names in lapply
#'
#' `retain_name` is a wrapper for `lapply` that retains the names
#' of the list passed to the appropriate function.
#' The function call is nearly identical to that of `lapply`.
#'
#' @param x The list to iterate over
#' @param f The function to apply
#' @param ... Additional arguments to pass to `f`
#' @return A list of the same length as `x` that is the output of
#' `lapply` with the names preserved
#' @export

retain_name <- function(x, f, ...) {
  list_name <- names(x)
  param <- list(X = x, FUN = f, ...)
  result <- do.call(lapply, param)
  names(result) <- list_name
  print(list_name)
  print(names(result))
  result
}
