#' Read a list of chemical descriptor files
#'
#' `read_desc_list` is a wrapper for `[read_desc()]` and `lapply`.
#' The function calls `[read_desc_helper()]` for additional error
#' handling. Acceptable files are RDS, CSV, and Excel files.
#'
#' Additionally, the list of the files read from the paths will
#' be named with the filenames minus the file extensions. This
#' can be controlled with `use_filename`
#'
#' @param path_list The list of locations of chemical descriptors
#' @param quiet Whether the function will produce messages.
#'   The default is `FALSE`.
#' @param shorten Whether to message the shortened file name.
#'   The default is `TRUE`.
#' @param use_filename Whether to use the file names to label the
#'   elements of the list. The default is `TRUE`.
#' @return A list of the files read from the paths
#' @export

read_desc_list <- function(path_list,
                           quiet = F,
                           shorten = T,
                           use_filename = T) {
  desc_list <- lapply(
    path_list,
    read_desc_helper,
    quiet = quiet,
    shorten = shorten
  )
  if (use_filename) {
    desc_name <- path_list %>%
      str_remove_all("[[:alpha:]]+/") %>%
      str_remove("\\.[[:alpha:]]+$")
    names(desc_list) <- desc_name
  }
  desc_list
}
