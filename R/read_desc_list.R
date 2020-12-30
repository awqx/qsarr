#' Read a list of chemical descriptor files
#'
#' `read_desc_list` is a wrapper for `[read_desc()]` and `lapply`.
#' The function calls `[read_desc_helper()]` for additional error
#' handling. Acceptable files are RDS, CSV, and Excel files.
#'
#' @param path_list The list of locations of chemical descriptors
#' @param quiet Whether the function will produce messages.
#'   The default is `FALSE`.
#' @param shorten Whether to message the shortened file name.
#'   The default is `TRUE`.
#' @return A list of the files read from the paths
#' @export

read_desc_list <- function(path_list, quiet = F, shorten = T) {
  desc_list <- lapply(
    path_list,
    read_desc_helper,
    quiet = quiet,
    shorten = shorten
  )
}
